package com.looseboxes.ratelimiter.builder;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.annotation.AnnotationProcessor;
import com.looseboxes.ratelimiter.annotation.ClassAnnotationProcessor;
import com.looseboxes.ratelimiter.annotation.NodeData;
import com.looseboxes.ratelimiter.annotation.NodeUtil;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.rates.Limit;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

class RateLimiterTreeBuilder<K> implements RateLimitersBuilder<K, Node<NodeData<RateLimiter<K>>>> {

    private final AtomicBoolean buildAttempted = new AtomicBoolean();
    private AnnotationProcessor<Class<?>> annotationProcessor;
    private Node<NodeData<Limit>> rootNode;
    private RateLimiterConfigBuilder<K, ?> rateLimiterConfigBuilder;
    private RateLimiterFactory<K> rateLimiterFactory;

    RateLimiterTreeBuilder() {
        this.rateLimiterConfigBuilder = RateLimiterConfig.builder();
    }

    @Override
    public Node<NodeData<RateLimiter<K>>> build(List<Class<?>> classes) {

        buildConfigs(classes);

        return rootNode.transform(null, (name, nodeData) ->
                new NodeData<>(nodeData.getSource(), createRateLimiter(nodeData.getValue())));
    }

    private RateLimiter<K> createRateLimiter(Limit limit) {
        Objects.requireNonNull(rateLimiterConfigBuilder);
        if(limit == null) {
            return RateLimiter.noop();
        }
        if(rateLimiterFactory == null) {
            rateLimiterFactory = new DefaultRateLimiterFactory<>();
        }
        return rateLimiterFactory.createRateLimiter(rateLimiterConfigBuilder.build(), limit);
    }

    private void buildConfigs(List<Class<?>> classes) {

        if(!buildAttempted.compareAndSet(false, true)) {
            throw new IllegalStateException("build() method may only be called once, per instance of this class");
        }

        if(annotationProcessor == null) {
            annotationProcessor(new ClassAnnotationProcessor());
        }

        if(rootNode == null) {
            rootNodeName("root-" + UUID.randomUUID());
        }

        annotationProcessor.process(rootNode, classes);
    }

    @Override public RateLimiterTreeBuilder<K> annotationProcessor(
            AnnotationProcessor<Class<?>> annotationProcessor) {
        this.annotationProcessor = annotationProcessor;
        return this;
    }

    @Override public RateLimiterTreeBuilder<K> rootNodeName(String name) {
        return rootNode(NodeUtil.createNode(name, null, null));
    }

    @Override public RateLimiterTreeBuilder<K> rootNode(Node<NodeData<Limit>> rootNode) {
        this.rootNode = rootNode;
        return this;
    }

    @Override public RateLimiterTreeBuilder<K> rateCache(RateCache<K, ?> rateCache) {
        this.rateLimiterConfigBuilder.rateCache((RateCache)rateCache);
        return this;
    }

    @Override public RateLimiterTreeBuilder<K> rateFactory(RateFactory rateFactory) {
        this.rateLimiterConfigBuilder.rateFactory(rateFactory);
        return this;
    }

    @Override
    public RateLimiterTreeBuilder<K> rateRecordedListener(RateRecordedListener rateRecordedListener) {
        this.rateLimiterConfigBuilder.rateRecordedListener(rateRecordedListener);
        return this;
    }

    @Override
    public RateLimiterTreeBuilder<K> rateLimiterFactory(RateLimiterFactory<K> rateLimiterFactory) {
        this.rateLimiterFactory = rateLimiterFactory;
        return this;
    }
}

