package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.RateFactory;
import com.looseboxes.ratelimiter.util.CompositeRate;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.Nullable;

import java.lang.reflect.Method;
import java.util.function.Predicate;

class RateLimitTreeBuilderForMethod extends RateLimitTreeBuilder<Method> {

    RateLimitTreeBuilderForMethod(IdProvider<Method, String> idProvider, RateFactory rateFactory) {
        super(idProvider, rateFactory);
    }

    @Override
    protected Node<NodeData<CompositeRate>> getOrCreateParent(
            @Nullable Node<NodeData<CompositeRate>> root, Method method,
            RateLimitGroup rateLimitGroup, RateLimit[] rateLimits) {

        Predicate<Node<NodeData<CompositeRate>>> testForDeclaringClass = node -> {
            NodeData<CompositeRate> nodeData = node == null ? null : node.getValueOrDefault(null);
            return nodeData != null && method.getDeclaringClass().equals(nodeData.getSource());
        };

        Node<NodeData<CompositeRate>> nodeForDeclaringClass = root == null ? null : root.findFirstChild(testForDeclaringClass).orElse(null);

        Node<NodeData<CompositeRate>> nodeForRateLimitGroup = findOrCreateNodeForRateLimitGroupOrNull(
                root, nodeForDeclaringClass, method, rateLimitGroup, rateLimits);

        return nodeForRateLimitGroup == null ? nodeForDeclaringClass : nodeForRateLimitGroup;
    }
}
