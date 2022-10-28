package com.looseboxes.ratelimiter.builder;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.annotation.AnnotationProcessor;
import com.looseboxes.ratelimiter.annotation.ClassAnnotationProcessor;
import com.looseboxes.ratelimiter.annotation.NodeData;
import com.looseboxes.ratelimiter.annotation.NodeUtil;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.RateConfigList;
import com.looseboxes.ratelimiter.util.Util;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

public class RateLimiterTreeBuilder<K> implements
        RateLimiterCollectionBuilder<Node<RateLimiter<K>>> {

    private final AtomicBoolean buildAttempted = new AtomicBoolean();
    private AnnotationProcessor<Class<?>> annotationProcessor;
    private Node<NodeData<RateConfigList>> rootNode;
    private DefaultRateLimiterConfig<K, ?> rateLimiterConfig;
    private RateLimiterFactory<K> rateLimiterFactory;

    public RateLimiterTreeBuilder() {
        this.rateLimiterConfig = new DefaultRateLimiterConfig<>();
    }

    @Override public Node<RateLimiter<K>> build(Class<?> clazz) {
        return build(Collections.singletonList(clazz));
    }

    @Override public Node<RateLimiter<K>> build(Class<?>... classes) {
        return build(Arrays.asList(classes));
    }

    @Override public Node<RateLimiter<K>> build(List<Class<?>> classes) {

        buildConfigs(classes);

        return rootNode.transform(null, (name, value) -> createRateLimiter(value.getValue()));
    }

    private RateLimiter<K> createRateLimiter(RateConfigList rateConfigList) {
        Objects.requireNonNull(rateLimiterConfig);
        if(rateConfigList == null) {
            return RateLimiter.noop();
        }
        if(rateLimiterFactory == null) {
            rateLimiterFactory = new DefaultRateLimiterFactory<>();
        }
        return rateLimiterFactory.createRateLimiter(rateLimiterConfig, rateConfigList);
    }

    private void buildConfigs(List<Class<?>> classes) {

        if(!buildAttempted.compareAndSet(false, true)) {
            throw new IllegalStateException("build() method may only be called once, per instance of this class");
        }

        if(annotationProcessor == null) {
            annotationProcessor(new ClassAnnotationProcessor());
        }

        if(rootNode == null) {
            rootNodeName(Util.randomUniqueId("root-", ""));
        }

        annotationProcessor.process(rootNode, classes);
    }

    public RateLimiterTreeBuilder<K> annotationProcessor(AnnotationProcessor<Class<?>> annotationProcessor) {
        this.annotationProcessor = annotationProcessor;
        return this;
    }

    public RateLimiterTreeBuilder<K> rootNodeName(String name) {
        return rootNode(NodeUtil.createNode(name, null, null));
    }

    public RateLimiterTreeBuilder<K> rootNode(Node<NodeData<RateConfigList>> rootNode) {
        this.rootNode = rootNode;
        return this;
    }

    public RateLimiterTreeBuilder<K> rateCache(RateCache<K, ?> rateCache) {
        this.rateLimiterConfig.rateCache((RateCache)rateCache);
        return this;
    }

    public RateLimiterTreeBuilder<K> rateFactory(RateFactory rateFactory) {
        this.rateLimiterConfig.rateFactory(rateFactory);
        return this;
    }

    public RateLimiterTreeBuilder<K> rateExceededListener(RateRecordedListener rateRecordedListener) {
        this.rateLimiterConfig.rateRecordedListener(rateRecordedListener);
        return this;
    }

    public RateLimiterTreeBuilder<K> rateLimiterFactory(RateLimiterFactory<K> rateLimiterFactory) {
        this.rateLimiterFactory = rateLimiterFactory;
        return this;
    }
}

