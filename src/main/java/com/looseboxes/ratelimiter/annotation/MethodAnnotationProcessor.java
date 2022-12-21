package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.Nullable;

import java.lang.reflect.Method;
import java.util.function.Predicate;

class MethodAnnotationProcessor extends AnnotationProcessor<Method> {

    MethodAnnotationProcessor(IdProvider<Method, String> idProvider) {
        super(idProvider);
    }

    @Override
    protected Node<NodeData<Bandwidths>> getOrCreateParent(
            @Nullable Node<NodeData<Bandwidths>> root, Method method,
            RateLimitGroup rateLimitGroup, RateLimit[] rateLimits) {

        Predicate<Node<NodeData<Bandwidths>>> testForDeclaringClass = node -> {
            NodeData<Bandwidths> nodeData = node == null ? null : node.getValueOrDefault(null);
            return nodeData != null && method.getDeclaringClass().equals(nodeData.getSource());
        };

        Node<NodeData<Bandwidths>> nodeForDeclaringClass = root == null ? null : root.findFirstChild(testForDeclaringClass).orElse(null);

        Node<NodeData<Bandwidths>> nodeForRateLimitGroup = findOrCreateNodeForRateLimitGroupOrNull(
                root, nodeForDeclaringClass, method, rateLimitGroup, rateLimits);

        return nodeForRateLimitGroup == null ? nodeForDeclaringClass : nodeForRateLimitGroup;
    }
}
