package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.Nullable;

import java.lang.reflect.Method;
import java.util.function.Predicate;

class MethodAnnotationProcessor<T> extends AnnotationProcessor<Method, T> {

    MethodAnnotationProcessor(IdProvider<Method, String> idProvider, Converter<T> converter) {
        super(idProvider, converter);
    }

    @Override
    protected Node<NodeData<T>> getOrCreateParent(
            @Nullable Node<NodeData<T>> root, Method method,
            RateLimitGroup rateLimitGroup, RateLimit[] rateLimits) {

        Predicate<Node<NodeData<T>>> testForDeclaringClass = node -> {
            NodeData<T> nodeData = node == null ? null : node.getValueOrDefault(null);
            return nodeData != null && method.getDeclaringClass().equals(nodeData.getSource());
        };

        Node<NodeData<T>> nodeForDeclaringClass = root == null ? null : root.findFirstChild(testForDeclaringClass).orElse(null);

        Node<NodeData<T>> nodeForRateLimitGroup = findOrCreateNodeForRateLimitGroupOrNull(
                root, nodeForDeclaringClass, method, rateLimitGroup, rateLimits);

        return nodeForRateLimitGroup == null ? nodeForDeclaringClass : nodeForRateLimitGroup;
    }
}
