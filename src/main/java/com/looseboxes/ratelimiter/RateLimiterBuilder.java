package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.annotation.AnnotationProcessor;
import com.looseboxes.ratelimiter.annotation.ClassAnnotationProcessor;
import com.looseboxes.ratelimiter.annotation.NodeValue;
import com.looseboxes.ratelimiter.annotation.NodeUtil;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.RateLimitConfig;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.BiFunction;

public final class RateLimiterBuilder {

    private final AtomicBoolean buildAttempted = new AtomicBoolean();
    private Node<NodeValue<RateLimitConfig>> rootNode;
    private AnnotationProcessor<Class<?>> annotationProcessor;

    public Node<RateLimiter<Object>> build(Class<?> clazz) {
        return build(Collections.singletonList(clazz));
    }

    public Node<RateLimiter<Object>> build(Class<?>... classes) {
        return build(Arrays.asList(classes));
    }

    public Node<RateLimiter<Object>> build(List<Class<?>> classes) {

        buildConfigs(classes);

        BiFunction<String, NodeValue<RateLimitConfig>, RateLimiter<Object>> valueConverter = (name, value) -> {
            RateLimitConfig config = value.getValue();
            return config == null ? RateLimiter.noop() : new SimpleRateLimiter<>(config);
        };

        return rootNode.transform(null, (name, value) -> name, valueConverter);
    }

    private void buildConfigs(List<Class<?>> classes) {

        requireBuildNotAttempted();

        if(annotationProcessor == null) {
            annotationProcessor(new ClassAnnotationProcessor());
        }

        if(rootNode == null) {
            rootNodeName(randomUniqueId());
        }

        annotationProcessor.process(rootNode, classes);
    }

    private void requireBuildNotAttempted() {
        synchronized (buildAttempted) {
            if (buildAttempted.get()) {
                throw new IllegalStateException("build method may only be called once per instance of this class");
            }
            buildAttempted.compareAndSet(false, true);
        }
    }

    private static final AtomicLong nonce = new AtomicLong();
    private String randomUniqueId() {
        return "root-" + Long.toHexString(System.currentTimeMillis()) + '-' + nonce.incrementAndGet();
    }

    public RateLimiterBuilder rootNodeName(String name) {
        return rootNode(NodeUtil.createNode(name, null, null));
    }

    public RateLimiterBuilder rootNode(Node<NodeValue<RateLimitConfig>> rootNode) {
        this.rootNode = rootNode;
        return this;
    }

    public RateLimiterBuilder annotationProcessor(AnnotationProcessor<Class<?>> annotationProcessor) {
        this.annotationProcessor = annotationProcessor;
        return this;
    }
}
