package com.looseboxes.ratelimiter.builder;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.annotation.AnnotationTreeBuilder;
import com.looseboxes.ratelimiter.annotation.NodeData;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.CompositeRate;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public interface RateLimitersBuilder<K, R> {

    static <K> RateLimitersBuilder<K, List<NodeData<RateLimiter<K>>>> list() {
        return new RateLimiterListBuilder<>();
    }

    static <K> RateLimitersBuilder<K, Node<NodeData<RateLimiter<K>>>> tree() {
        return new RateLimiterTreeBuilder<>();
    }

    default R build(Class<?> clazz) {
        return build(Collections.singletonList(clazz));
    }

    default R build(Class<?>... classes) {
        return build(Arrays.asList(classes));
    }

    R build(List<Class<?>> classes);

    RateLimitersBuilder<K, R> annotationProcessor(
            AnnotationTreeBuilder<Class<?>> annotationTreeBuilder);

    RateLimitersBuilder<K, R> rootNodeName(String name) ;

    RateLimitersBuilder<K, R> rootNode(Node<NodeData<CompositeRate>> rootNode);

    RateLimitersBuilder<K, R> rateCache(RateCache<K, ?> rateCache);

    RateLimitersBuilder<K, R> rateFactory(RateFactory rateFactory);

    RateLimitersBuilder<K, R> rateRecordedListener(RateRecordedListener rateRecordedListener);

    RateLimitersBuilder<K, R> rateLimiterFactory(RateLimiterFactory<K> rateLimiterFactory);
}
