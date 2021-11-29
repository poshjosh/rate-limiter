package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;

import java.lang.reflect.Method;
import java.util.function.Predicate;

public class MethodAnnotationProcessor extends AnnotationProcessorImpl<Method>{

    public MethodAnnotationProcessor() {
        this(new MethodNameProvider());
    }

    public MethodAnnotationProcessor(IdProvider<Method, String> idProvider) {
        super(idProvider);
    }

    @Override
    protected Node<NodeData> getOrCreateParent(
            Node<NodeData> root, Method method, RateLimitGroup rateLimitGroup, RateLimit[] rateLimits) {

        Predicate<Node<NodeData>> testForDeclaringClass = node -> {
            NodeData nodeData = node.getValueOrDefault(null);
            return nodeData != null && method.getDeclaringClass().equals(nodeData.getSource());
        };

        Node<NodeData> nodeForDeclaringClass = root.findFirstChild(testForDeclaringClass).orElse(null);

        Node<NodeData> nodeForRateLimitGroup = findOrCreateNodeForRateLimitGroupOrNull(
                root, nodeForDeclaringClass, method, rateLimitGroup, rateLimits);

        return nodeForRateLimitGroup == null ? nodeForDeclaringClass : nodeForRateLimitGroup;
    }
}
