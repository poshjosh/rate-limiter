package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.annotation.NodeData;
import com.looseboxes.ratelimiter.node.BreadthFirstNodeVisitor;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.Matcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.function.Consumer;

public class PatternMatchingRateLimiter<R> implements RateLimiter<R>{

    private enum RateLimitResult{SUCCESS, FAILURE, NOMATCH, NOOP}

    private static final Logger log = LoggerFactory.getLogger(PatternMatchingRateLimiter.class);
    
    public interface MatcherProvider<T>{
        Matcher<T, ?> getMatcher(String nodeName, NodeData<RateLimiter<?>> nodeData);
    }

    private final MatcherProvider<R> matcherProvider;
    private final Node<NodeData<RateLimiter<?>>> rootNode;
    private final List<Node<NodeData<RateLimiter<?>>>> leafNodes;
    private final boolean firstMatchOnly;

    public PatternMatchingRateLimiter(
            Node<NodeData<RateLimiter<?>>> rootNode,
            boolean firstMatchOnly) {
        this((nodeName, nodeData) -> Matcher.identity(), rootNode, firstMatchOnly);
    }

    public PatternMatchingRateLimiter(MatcherProvider<R> matcherProvider,
                                      Node<NodeData<RateLimiter<?>>> rootNode,
                                      boolean firstMatchOnly) {
        this.matcherProvider = Objects.requireNonNull(matcherProvider);
        this.rootNode = Objects.requireNonNull(rootNode);
        Set<Node<NodeData<RateLimiter<?>>>> set = new LinkedHashSet<>();
        collectLeafNodes(this.rootNode, set::add);
        this.leafNodes = new LinkedList<>(set);
        this.firstMatchOnly = firstMatchOnly;
    }

    private <T> void collectLeafNodes(Node<T> root, Consumer<Node<T>> collector) {
        new BreadthFirstNodeVisitor<>(Node::isLeaf, collector).accept(root);
    }

    @Override
    public boolean consume(Object context, R request, int amount) {

        int globalFailureCount = 0;

        for(Node<NodeData<RateLimiter<?>>> node : leafNodes) {

            int nodeSuccessCount = 0;

            while(node != rootNode && node != null && node.hasNodeValue()) {

                final RateLimitResult result = increment(request, amount, node);

                switch(result) {
                    case SUCCESS: ++nodeSuccessCount; break;
                    case FAILURE: ++globalFailureCount; break;
                    case NOMATCH:
                    case NOOP:
                        break;
                    default: throw new IllegalArgumentException();
                }

                if(!RateLimitResult.SUCCESS.equals(result)) {
                    break;
                }

                node = node.getParentOrDefault(null);
            }

            if(firstMatchOnly && nodeSuccessCount > 0) {
                break;
            }
        }

        return globalFailureCount == 0;
    }

    private RateLimitResult increment(R request, int amount, Node<NodeData<RateLimiter<?>>> node) {

        final String nodeName = node.getName();
        final NodeData<RateLimiter<?>> nodeData = node.getValueOptional().orElseThrow(NullPointerException::new);
        final RateLimiter<?> rateLimiter = nodeData.getValue();
        if(log.isTraceEnabled()) {
            log.trace("Name: {}, rate-limiter: {}", nodeName, rateLimiter);
        }

        if(rateLimiter == RateLimiter.NO_OP) {
            return RateLimitResult.NOOP;
        }

        Matcher<R, ?> matcher = matcherProvider.getMatcher(nodeName, nodeData);

        final Object key = matcher.matchOrNull(request);

        if(log.isTraceEnabled()) {
            log.trace("Name: {}, matched: {}, matcher: {}", nodeName, key != null, matcher);
        }

        if(key == null) {
            return RateLimitResult.NOMATCH;
        }

        return ((RateLimiter<Object>)rateLimiter).consume(request, key, amount) ? RateLimitResult.SUCCESS : RateLimitResult.FAILURE;
    }
}