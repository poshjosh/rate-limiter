package com.looseboxes.ratelimiter.builder;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.annotation.AnnotationProcessor;
import com.looseboxes.ratelimiter.annotation.NodeData;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.node.BreadthFirstNodeVisitor;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.bandwidths.Bandwidths;

import java.util.*;

class RateLimiterListBuilder<K> implements RateLimitersBuilder<K, List<NodeData<RateLimiter<K>>>> {

    private final RateLimiterTreeBuilder<K> rateLimiterTreeBuilder;

    RateLimiterListBuilder() {
        this(new RateLimiterTreeBuilder<>());
    }

    RateLimiterListBuilder(RateLimiterTreeBuilder<K> rateLimiterTreeBuilder) {
        this.rateLimiterTreeBuilder = rateLimiterTreeBuilder;
    }

    public List<NodeData<RateLimiter<K>>> build(List<Class<?>> classes) {
        Node<NodeData<RateLimiter<K>>> node = rateLimiterTreeBuilder.build(classes);
        return toList(node);
    }

    private List<NodeData<RateLimiter<K>>> toList(Node<NodeData<RateLimiter<K>>> node) {
        // Do we need to prevent duplicates here?
        List<NodeData<RateLimiter<K>>> result = new ArrayList<>();
        new BreadthFirstNodeVisitor<NodeData<RateLimiter<K>>>(n -> n.getValueOptional().ifPresent(result::add)).accept(node);
        return Collections.unmodifiableList(result);
    }

    public RateLimiterListBuilder<K> annotationProcessor(
            AnnotationProcessor<Class<?>, Bandwidths> annotationProcessor) {
        this.rateLimiterTreeBuilder.annotationProcessor(annotationProcessor);
        return this;
    }

    public RateLimiterListBuilder<K> rootNodeName(String name) {
        this.rateLimiterTreeBuilder.rootNodeName(name);
        return this;
    }

    public RateLimiterListBuilder<K> rootNode(Node<NodeData<Bandwidths>> rootNode) {
        this.rateLimiterTreeBuilder.rootNode(rootNode);
        return this;
    }

    public RateLimiterListBuilder<K> rateCache(RateCache<K, ?> rateCache) {
        this.rateLimiterTreeBuilder.rateCache(rateCache);
        return this;
    }

    public RateLimiterListBuilder<K> rateRecordedListener(RateRecordedListener rateRecordedListener) {
        this.rateLimiterTreeBuilder.rateRecordedListener(rateRecordedListener);
        return this;
    }

    public RateLimiterListBuilder<K> rateLimiterFactory(RateLimiterFactory<K> rateLimiterFactory) {
        this.rateLimiterTreeBuilder.rateLimiterFactory(rateLimiterFactory);
        return this;
    }
}
