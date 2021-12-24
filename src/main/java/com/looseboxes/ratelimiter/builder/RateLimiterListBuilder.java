package com.looseboxes.ratelimiter.builder;

import com.looseboxes.ratelimiter.RateRecordedListener;
import com.looseboxes.ratelimiter.RateFactory;
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.RateLimiterFactory;
import com.looseboxes.ratelimiter.annotation.AnnotationProcessor;
import com.looseboxes.ratelimiter.annotation.NodeValue;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.node.BreadthFirstNodeVisitor;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.RateConfigList;

import java.util.*;

public class RateLimiterListBuilder<K> implements
        RateLimiterCollectionBuilder<List<RateLimiter<K>>> {

    private final RateLimiterTreeBuilder<K> rateLimiterTreeBuilder;

    public RateLimiterListBuilder() {
        this(new RateLimiterTreeBuilder<>());
    }

    public RateLimiterListBuilder(RateLimiterTreeBuilder<K> rateLimiterTreeBuilder) {
        this.rateLimiterTreeBuilder = rateLimiterTreeBuilder;
    }

    public List<RateLimiter<K>> build(Class<?> clazz) {
        Node<RateLimiter<K>> node = rateLimiterTreeBuilder.build(clazz);
        return toList(node);
    }

    public List<RateLimiter<K>> build(Class<?>... classes) {
        Node<RateLimiter<K>> node = rateLimiterTreeBuilder.build(classes);
        return toList(node);
    }

    public List<RateLimiter<K>> build(List<Class<?>> classes) {
        Node<RateLimiter<K>> node = rateLimiterTreeBuilder.build(classes);
        return toList(node);
    }

    private List<RateLimiter<K>> toList(Node<RateLimiter<K>> node) {
        List<RateLimiter<K>> result = new ArrayList<>();
        new BreadthFirstNodeVisitor<RateLimiter<K>>(n -> n.getValueOptional().ifPresent(result::add)).accept(node);
        return Collections.unmodifiableList(result);
    }

    public RateLimiterListBuilder<K> annotationProcessor(
            AnnotationProcessor<Class<?>> annotationProcessor) {
        this.rateLimiterTreeBuilder.annotationProcessor(annotationProcessor);
        return this;
    }

    public RateLimiterListBuilder<K> rootNodeName(String name) {
        this.rateLimiterTreeBuilder.rootNodeName(name);
        return this;
    }

    public RateLimiterListBuilder<K> rootNode(Node<NodeValue<RateConfigList>> rootNode) {
        this.rateLimiterTreeBuilder.rootNode(rootNode);
        return this;
    }

    public RateLimiterListBuilder<K> rateCache(RateCache<K, ?> rateCache) {
        this.rateLimiterTreeBuilder.rateCache(rateCache);
        return this;
    }

    public RateLimiterListBuilder<K> rateFactory(RateFactory rateFactory) {
        this.rateLimiterTreeBuilder.rateFactory(rateFactory);
        return this;
    }

    public RateLimiterListBuilder<K> rateExceededListener(RateRecordedListener rateRecordedListener) {
        this.rateLimiterTreeBuilder.rateExceededListener(rateRecordedListener);
        return this;
    }

    public RateLimiterListBuilder<K> rateLimiterFactory(RateLimiterFactory<K> rateLimiterFactory) {
        this.rateLimiterTreeBuilder.rateLimiterFactory(rateLimiterFactory);
        return this;
    }
}
