package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.NodeData;

import java.lang.reflect.Method;
import java.util.Objects;
import java.util.function.Predicate;

public final class MethodAnnotationProcessor extends AnnotationProcessorImpl<Method>{

    private final IdProvider<Class<?>, String> classIdProvider;

    public MethodAnnotationProcessor() {
        this(new ClassNameProvider(), new MethodNameProvider());
    }

    public MethodAnnotationProcessor(IdProvider<Class<?>, String> classIdProvider, IdProvider<Method, String> idProvider) {
        super(idProvider);
        this.classIdProvider = Objects.requireNonNull(classIdProvider);
    }

    @Override
    protected Node<NodeData> getOrCreateParent(
            Node<NodeData> root, Method method, RateLimitGroup rateLimitGroup, RateLimit[] rateLimits) {

        Predicate<Node<NodeData>> testForDeclaringClass = node -> {
            NodeData nodeData = node.getValueOrDefault(null);
            return nodeData != null && method.getDeclaringClass().equals(nodeData.getSource());
        };

        Node<NodeData> nodeForDeclaringClass = root.findFirstChild(testForDeclaringClass)
                .orElseGet(() -> createNodeForDeclaringClass(root, method, rateLimitGroup, rateLimits));

        Node<NodeData> nodeForRateLimitGroup = findOrCreateNodeForRateLimitGroupOrNull(
                root, nodeForDeclaringClass, rateLimitGroup, rateLimits);

        return nodeForRateLimitGroup == null ? nodeForDeclaringClass : nodeForRateLimitGroup;
    }

    private Node<NodeData> createNodeForDeclaringClass(
            Node<NodeData> root, Method method,
            RateLimitGroup rateLimitGroup, RateLimit[] rateLimits) {
        Class<?> declaringClass = method.getDeclaringClass();
        String name = classIdProvider.getId(declaringClass);
        return createNodeForElementOrNull(root, name, declaringClass, rateLimitGroup, rateLimits);
    }
}
