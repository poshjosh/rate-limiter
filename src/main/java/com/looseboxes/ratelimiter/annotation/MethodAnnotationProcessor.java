package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.Nullable;
import com.looseboxes.ratelimiter.util.RateLimitConfig;

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
    protected Node<NodeValue<RateLimitConfig>> getOrCreateParent(
            @Nullable Node<NodeValue<RateLimitConfig>> root, Method method,
            RateLimitGroup rateLimitGroup, RateLimit[] rateLimits) {

        Predicate<Node<NodeValue<RateLimitConfig>>> testForDeclaringClass = node -> {
            NodeValue nodeValue = node == null ? null : node.getValueOrDefault(null);
            return nodeValue != null && method.getDeclaringClass().equals(nodeValue.getSource());
        };

        Node<NodeValue<RateLimitConfig>> nodeForDeclaringClass = root == null ? null : root.findFirstChild(testForDeclaringClass).orElse(null);

        Node<NodeValue<RateLimitConfig>> nodeForRateLimitGroup = findOrCreateNodeForRateLimitGroupOrNull(
                root, nodeForDeclaringClass, method, rateLimitGroup, rateLimits);

        return nodeForRateLimitGroup == null ? nodeForDeclaringClass : nodeForRateLimitGroup;
    }
}
