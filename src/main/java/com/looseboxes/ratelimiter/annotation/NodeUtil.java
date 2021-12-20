package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.NodeImpl;
import com.looseboxes.ratelimiter.util.RateLimitConfig;

import java.util.*;

public final class NodeUtil {

    private static final Object sourceForPropertyNodes = new Object();

    private static final Object sourceForGroupNodes = new Object();

    private NodeUtil(){}

    public static boolean isPropertyNodeData(NodeValue nodeValue) {
        return nodeValue.getSource() == sourceForPropertyNodes;
    }

    public static void createNodes(Node<NodeValue<RateLimitConfig>> parent, Map<String, RateLimitConfig> rateLimitConfigs) {
        Set<Map.Entry<String, RateLimitConfig>> entrySet = rateLimitConfigs.entrySet();
        for (Map.Entry<String, RateLimitConfig> entry : entrySet) {
            String name = entry.getKey();
            if(name.equals(parent.getName())) {
                throw new IllegalStateException("Parent and child nodes both have the same name: " + name);
            }
            createNode(parent, name, sourceForPropertyNodes, entry.getValue());
        }
    }

    public static Node<NodeValue<RateLimitConfig>> createGroupNode(Node<NodeValue<RateLimitConfig>> parent, String name, RateLimitConfig rateLimitConfig) {
        return createNode(parent, name, sourceForGroupNodes, rateLimitConfig);
    }

    public static Node<NodeValue<RateLimitConfig>> createNode(
            Node<NodeValue<RateLimitConfig>> parent, String name,
            Object source, RateLimitConfig rateLimitConfig) {
        NodeValue nodeValue = new NodeValue(source, rateLimitConfig);
        return createNode(name, nodeValue, parent);
    }

    public static Node<NodeValue<RateLimitConfig>> createNode(String name) {
        return createNode(name, null, null);
    }

    public static Node<NodeValue<RateLimitConfig>> createNode(String name, NodeValue nodeValue, Node<NodeValue<RateLimitConfig>> parent) {
        return new NodeImpl<>(name, nodeValue, parent);
    }

    public static AnnotationProcessingException newExceptionForRequiredValue() {
        return new AnnotationProcessingException("Only the root node may have no value");
    }
}
