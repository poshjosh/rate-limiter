package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.Nullable;
import com.looseboxes.ratelimiter.util.RateConfigList;

import java.lang.reflect.Method;
import java.util.function.Predicate;

public class MethodAnnotationProcessor extends AbstractAnnotationProcessor<Method> {

    public MethodAnnotationProcessor() {
        this(new MethodNameProvider());
    }

    public MethodAnnotationProcessor(IdProvider<Method, String> idProvider) {
        super(idProvider);
    }

    @Override
    protected Node<NodeValue<RateConfigList>> getOrCreateParent(
            @Nullable Node<NodeValue<RateConfigList>> root, Method method,
            RateLimitGroup rateLimitGroup, RateLimit[] rateLimits) {

        Predicate<Node<NodeValue<RateConfigList>>> testForDeclaringClass = node -> {
            NodeValue nodeValue = node == null ? null : node.getValueOrDefault(null);
            return nodeValue != null && method.getDeclaringClass().equals(nodeValue.getSource());
        };

        Node<NodeValue<RateConfigList>> nodeForDeclaringClass = root == null ? null : root.findFirstChild(testForDeclaringClass).orElse(null);

        Node<NodeValue<RateConfigList>> nodeForRateLimitGroup = findOrCreateNodeForRateLimitGroupOrNull(
                root, nodeForDeclaringClass, method, rateLimitGroup, rateLimits);

        return nodeForRateLimitGroup == null ? nodeForDeclaringClass : nodeForRateLimitGroup;
    }
}
