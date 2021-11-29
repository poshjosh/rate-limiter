package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.annotation.AnnotationProcessor;
import com.looseboxes.ratelimiter.annotation.ClassAnnotationProcessor;
import com.looseboxes.ratelimiter.annotation.NodeData;
import com.looseboxes.ratelimiter.annotation.NodeUtil;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.RateLimitConfig;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

public final class RateLimiterBuilder {

    private final AtomicBoolean buildAttempted = new AtomicBoolean();
    private Node<NodeData> rootNode;
    private AnnotationProcessor<Class<?>> annotationProcessor;

    public RateLimiterBuilder() { }

    public Optional<RateLimiter<Object>> build(Class<?> clazz) {
        return build(Collections.singletonList(clazz)).stream().findFirst();
    }

    public List<RateLimiter<Object>> build(Class<?>... classes) {
        return build(Arrays.asList(classes));
    }

    public List<RateLimiter<Object>> build(List<Class<?>> classes) {
        return buildConfigs(classes).stream()
                .map(DefaultRateLimiter::new)
                .collect(Collectors.toList());
    }

    private List<RateLimitConfig> buildConfigs(List<Class<?>> classes) {

        requireBuildNotAttempted();

        if(annotationProcessor == null) {
            annotationProcessor(new ClassAnnotationProcessor());
        }

        if(rootNode == null) {
            rootNodeName(randomUniqueId());
        }

        List<Node<NodeData>> nodes = new ArrayList<>();
        annotationProcessor.process(rootNode, classes, (source, node) -> nodes.add(node));
        return nodes.stream()
                .filter(Objects::nonNull)
                .map(node -> node.getValueOrDefault(null))
                .filter(Objects::nonNull)
                .map(NodeData::getConfig)
                .collect(Collectors.toList());
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

    public RateLimiterBuilder rootNode(Node<NodeData> rootNode) {
        this.rootNode = rootNode;
        return this;
    }

    public RateLimiterBuilder annotationProcessor(AnnotationProcessor<Class<?>> annotationProcessor) {
        this.annotationProcessor = annotationProcessor;
        return this;
    }
}
