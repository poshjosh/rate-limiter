package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.NodeImpl;
import com.looseboxes.ratelimiter.util.RateLimitConfig;

import java.util.*;

public final class NodeUtil {

    private static final Object sourceForPropertyNodes = new Object();

    private static final Object sourceForGroupNodes = new Object();

    private NodeUtil(){}

    public static boolean isPropertyNodeData(NodeData nodeData) {
        return nodeData.getSource() == sourceForPropertyNodes;
    }

    public static void createNodes(Node<NodeData> parent, Map<String, RateLimitConfig> rateLimitConfigs) {
        Set<Map.Entry<String, RateLimitConfig>> entrySet = rateLimitConfigs.entrySet();
        for (Map.Entry<String, RateLimitConfig> entry : entrySet) {
            String name = entry.getKey();
            if(name.equals(parent.getName())) {
                throw new IllegalStateException("Parent and child nodes both have the same name: " + name);
            }
            createNode(parent, name, sourceForPropertyNodes, entry.getValue());
        }
    }

    public static Node<NodeData> createGroupNode(Node<NodeData> parent, String name, RateLimitConfig rateLimitConfig) {
        return createNode(parent, name, sourceForGroupNodes, rateLimitConfig);
    }

    public static Node<NodeData> createNode(
            Node<NodeData> parent, String name,
            Object source, RateLimitConfig rateLimitConfig) {
        NodeData nodeData = new NodeData(source, rateLimitConfig);
        return createNode(name, nodeData, parent);
    }

    public static Node<NodeData> createNode(String name) {
        return createNode(name, null, null);
    }

    public static Node<NodeData> createNode(String name, NodeData nodeData, Node<NodeData> parent) {
        return new NodeImpl<>(name, nodeData, parent);
    }

    public static AnnotationProcessingException newExceptionForRequiredValue() {
        return new AnnotationProcessingException("Only the root node may have no value");
    }
}
