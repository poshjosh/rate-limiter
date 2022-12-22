package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.annotation.NodeData;
import com.looseboxes.ratelimiter.node.BreadthFirstNodeVisitor;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.Matcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.function.Function;

public class PatternMatchingRateLimiter<R> implements RateLimiter<R>{

    private enum RateLimitResult{SUCCESS, FAILURE, NOMATCH}

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
    public boolean tryConsume(Object context, R request, int permits, long timeout, TimeUnit unit) {

        Function<Node<NodeData<RateLimiter<?>>>, RateLimitResult> consumePermits =
                node -> consume(request, permits, timeout, unit, node);

        return visitNodes(consumePermits);
    }

    public List<RateLimiter<?>> collectRateLimiters(R request) {

        final List<RateLimiter<?>> rateLimiters = new ArrayList<>();

        Function<Node<NodeData<RateLimiter<?>>>, RateLimitResult> collectRateLimiters = node -> {

            final RateLimiter<?> rateLimiter = getRateLimiter(node);

            if(rateLimiter == RateLimiter.NO_OP) {
                return RateLimitResult.NOMATCH;
            }

            Object key = matchingKeyOrNull(request, node);

            if(key == null) {
                return RateLimitResult.NOMATCH;
            }

            rateLimiters.add(rateLimiter);

            return RateLimitResult.SUCCESS;
        };

        visitNodes(collectRateLimiters);

        return Collections.unmodifiableList(rateLimiters);
    }

    public boolean visitNodes(Function<Node<NodeData<RateLimiter<?>>>, RateLimitResult> visitor) {

        int globalFailureCount = 0;

        for(Node<NodeData<RateLimiter<?>>> node : leafNodes) {

            int nodeSuccessCount = 0;

            while(node != rootNode && node != null && node.hasNodeValue()) {

                final RateLimitResult result = visitor.apply(node);

                switch(result) {
                    case SUCCESS: ++nodeSuccessCount; break;
                    case FAILURE: ++globalFailureCount; break;
                    case NOMATCH:
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

    private RateLimitResult consume(R request, int permits, long timeout, TimeUnit unit, Node<NodeData<RateLimiter<?>>> node) {

        final RateLimiter<?> rateLimiter = getRateLimiter(node);

        if(rateLimiter == RateLimiter.NO_OP) {
            return RateLimitResult.NOMATCH;
        }

        final Object key = matchingKeyOrNull(request, node);

        if(key == null) {
            return RateLimitResult.NOMATCH;
        }

        return ((RateLimiter<Object>)rateLimiter).tryConsume(request, key, permits, timeout, unit) ? RateLimitResult.SUCCESS : RateLimitResult.FAILURE;
    }

    private Object matchingKeyOrNull(R request, Node<NodeData<RateLimiter<?>>> node) {
        final String nodeName = node.getName();
        final NodeData<RateLimiter<?>> nodeData = node.getValueOptional().orElseThrow(NullPointerException::new);
        final Matcher<R, ?> matcher = matcherProvider.getMatcher(nodeName, nodeData);
        final Object key = matcher.matchingKeyOrNull(request);
        if(log.isTraceEnabled()) {
            log.trace("Name: {}, matched: {}, matcher: {}", nodeName, key != null, matcher);
        }
        return key;
    }

    private RateLimiter<?> getRateLimiter(Node<NodeData<RateLimiter<?>>> node) {
        final RateLimiter<?> rateLimiter = node.getValueOrDefault(null).getValue();
        if(log.isTraceEnabled()) {
            log.trace("Name: {}, rate-limiter: {}", node.getName(), rateLimiter);
        }
        return rateLimiter;
    }
}
