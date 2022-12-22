package com.looseboxes.ratelimiter.builder;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.annotation.AnnotationProcessor;
import com.looseboxes.ratelimiter.annotation.NodeData;
import com.looseboxes.ratelimiter.annotation.NodeUtil;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.Rates;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

class RateLimiterTreeBuilder<K> implements RateLimitersBuilder<K, Node<NodeData<RateLimiter<K>>>> {

    private final AtomicBoolean buildAttempted = new AtomicBoolean();
    private AnnotationProcessor<Class<?>, Rates> annotationProcessor;
    private Node<NodeData<Rates>> rootNode;
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

        if(annotationProcessor == null) {
            annotationProcessor(AnnotationProcessor.newInstance());
        }

        if(rootNode == null) {
            rootNodeName("root-" + UUID.randomUUID());
        }

        annotationProcessor.process(rootNode, classes);
    }

    private RateLimiter<K> createRateLimiter(Rates rates) {
        if(rates == null || !rates.hasLimits()) {
            return RateLimiter.noop();
        }
        if(rateLimiterFactory == null) {
            rateLimiterFactory = RateLimiterFactory.newInstance();
        }
        return rateLimiterFactory.createRateLimiter(rateLimiterConfig, rates);
    }

    @Override
    public RateLimiterTreeBuilder<K> annotationProcessor(
            AnnotationProcessor<Class<?>, Rates> annotationProcessor) {
        this.annotationProcessor = annotationProcessor;
        return this;
    }

    @Override
    public RateLimiterTreeBuilder<K> rootNodeName(String name) {
        return rootNode(NodeUtil.createNode(name, null, null));
    }

    @Override
    public RateLimiterTreeBuilder<K> rootNode(Node<NodeData<Rates>> rootNode) {
        this.rootNode = rootNode;
        return this;
    }

    @Override
    public RateLimiterTreeBuilder<K> rateCache(RateCache<K, ?> rateCache) {
        this.rateLimiterConfigBuilder.rateCache((RateCache)rateCache);
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

