package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.PatternMatchingRateLimiter;
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.RateLimiterConfig;
import com.looseboxes.ratelimiter.RateLimiterFactory;
import com.looseboxes.ratelimiter.node.BreadthFirstNodeVisitor;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.Rates;

import java.util.HashMap;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Consumer;

public final class RateLimiterFromAnnotationFactory<K, V> {

    public static <K, V> RateLimiterFromAnnotationFactory<K, V> of() {
        return new RateLimiterFromAnnotationFactory<>(
                AnnotationProcessor.ofRates(), RateLimiterFactory.of(), RateLimiterConfig.of());
    }

    private AnnotationProcessor<Class<?>, Rates> annotationProcessor;
    private RateLimiterFactory<K> rateLimiterFactory;
    private RateLimiterConfig<K, V> rateLimiterConfig;

    private RateLimiterFromAnnotationFactory(AnnotationProcessor<Class<?>, Rates> annotationProcessor, RateLimiterFactory<K> rateLimiterFactory, RateLimiterConfig<K, V> rateLimiterConfig) {
        this.annotationProcessor = annotationProcessor;
        this.rateLimiterFactory = rateLimiterFactory;
        this.rateLimiterConfig = rateLimiterConfig;
    }

    public Map<Object, RateLimiter<K>> createMap(Class<?>... sources) {
        Node<NodeValue<RateLimiter<K>>> rootNode = createNode(sources);
        Map<Object, RateLimiter<K>> rateLimiterMap = new HashMap<>(sources.length);
        Consumer<Node<NodeValue<RateLimiter<K>>>> nodeConsumer = node -> {
            NodeValue<RateLimiter<K>> nodeValue = node.getValueOrDefault(null);
            if (nodeValue == null) {
                return;
            }
            RateLimiter<K> rateLimiter = nodeValue.getValue();
            if (rateLimiter == null) {
                return;
            }
            rateLimiterMap.put(nodeValue.getSource(), rateLimiter);
        };
        new BreadthFirstNodeVisitor<>(nodeConsumer).accept(rootNode);
        return rateLimiterMap;
    }

    public RateLimiter<K> create(Class<?>... sources) {
        return new PatternMatchingRateLimiter<K>((Node) createNode(sources), false);
    }

    public Node<NodeValue<RateLimiter<K>>> createNode(Class<?>... sources) {

        Node<NodeValue<Rates>> ratesRootNode = Node.of("root");

        annotationProcessor.processAll(ratesRootNode, sources);

        BiFunction<String, NodeValue<Rates>, NodeValue<RateLimiter<K>>> transformer = (nodeName, nodeValue) -> {
            return nodeValue.withValue(rateLimiterFactory.createNew(rateLimiterConfig, nodeValue.getValue()));
        };

        return ratesRootNode.transform(transformer);
    }

    public RateLimiterFromAnnotationFactory<K, V> annotationProcessor(AnnotationProcessor<Class<?>, Rates> annotationProcessor) {
        this.annotationProcessor = annotationProcessor;
        return this;
    }

    public RateLimiterFromAnnotationFactory<K, V> rateLimiterFactory(RateLimiterFactory<K> rateLimiterFactory) {
        this.rateLimiterFactory = rateLimiterFactory;
        return this;
    }

    public RateLimiterFromAnnotationFactory<K, V> rateLimiterConfig(RateLimiterConfig<K, V> rateLimiterConfig) {
        this.rateLimiterConfig = rateLimiterConfig;
        return this;
    }
}
