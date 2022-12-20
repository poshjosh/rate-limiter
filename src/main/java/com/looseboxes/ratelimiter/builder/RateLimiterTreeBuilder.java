package com.looseboxes.ratelimiter.builder;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.annotation.AnnotationTreeBuilder;
import com.looseboxes.ratelimiter.annotation.NodeData;
import com.looseboxes.ratelimiter.annotation.NodeUtil;
import com.looseboxes.ratelimiter.annotation.RateLimitTreeBuilder;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.CompositeRate;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

class RateLimiterTreeBuilder<K> implements RateLimitersBuilder<K, Node<NodeData<RateLimiter<K>>>> {

    private final AtomicBoolean buildAttempted = new AtomicBoolean();
    private AnnotationTreeBuilder<Class<?>> annotationTreeBuilder;
    private Node<NodeData<CompositeRate>> rootNode;
    private RateLimiterConfig.Builder<K, ?> rateLimiterConfigBuilder;
    private RateLimiterConfig<K, ?> rateLimiterConfig;
    private RateLimiterFactory<K> rateLimiterFactory;

    RateLimiterTreeBuilder() {
        this.rateLimiterConfigBuilder = RateLimiterConfig.builder();
    }

    @Override
    public Node<NodeData<RateLimiter<K>>> build(List<Class<?>> classes) {

        buildTree(classes);

        return rootNode.transform(null, (name, nodeData) ->
                new NodeData<>(nodeData.getSource(), createRateLimiter(nodeData.getValue())));
    }

    private void buildTree(List<Class<?>> classes) {

        if(!buildAttempted.compareAndSet(false, true)) {
            throw new IllegalStateException("build() method may only be called once, per instance of this class");
        }

        rateLimiterConfig = Objects.requireNonNull(rateLimiterConfigBuilder).build();

        if(annotationTreeBuilder == null) {
            annotationProcessor(RateLimitTreeBuilder.newInstance(rateLimiterConfig.getRateFactory()));
        }

        if(rootNode == null) {
            rootNodeName("root-" + UUID.randomUUID());
        }

        annotationTreeBuilder.build(rootNode, classes);
    }

    private RateLimiter<K> createRateLimiter(CompositeRate limit) {
        if(limit == null) {
            return RateLimiter.noop();
        }
        if(rateLimiterFactory == null) {
            rateLimiterFactory = RateLimiterFactory.newInstance();
        }
        return rateLimiterFactory.createRateLimiter(rateLimiterConfig, limit);
    }

    @Override
    public RateLimiterTreeBuilder<K> annotationProcessor(
            AnnotationTreeBuilder<Class<?>> annotationTreeBuilder) {
        this.annotationTreeBuilder = annotationTreeBuilder;
        return this;
    }

    @Override
    public RateLimiterTreeBuilder<K> rootNodeName(String name) {
        return rootNode(NodeUtil.createNode(name, null, null));
    }

    @Override
    public RateLimiterTreeBuilder<K> rootNode(Node<NodeData<CompositeRate>> rootNode) {
        this.rootNode = rootNode;
        return this;
    }

    @Override
    public RateLimiterTreeBuilder<K> rateCache(RateCache<K, ?> rateCache) {
        this.rateLimiterConfigBuilder.rateCache((RateCache)rateCache);
        return this;
    }

    @Override
    public RateLimiterTreeBuilder<K> rateFactory(RateFactory rateFactory) {
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

