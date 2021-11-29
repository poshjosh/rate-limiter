package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.NodeImpl;
import com.looseboxes.ratelimiter.util.RateLimitConfig;

import java.util.*;

public final class NodeUtil {

    public static final String ROOT_NODE_NAME = "root";

    private static Node<NodeData> rootNode;

    private static final Object sourceForPropertyNodes = new Object();

    private static final Object sourceForGroupNodes = new Object();

    private NodeUtil(){}

    public static Node<NodeData> getOrCreateRootNode() {
        if(rootNode == null) {
            rootNode = new NodeImpl<>(ROOT_NODE_NAME, null, null);
        }
        return rootNode;
    }

    public static boolean isRootNode(String name, NodeData nodeData) {
        return ROOT_NODE_NAME.equals(name) && nodeData == null;
    }

    public static boolean isPropertyNodeData(NodeData nodeData) {
        return nodeData.getSource() == sourceForPropertyNodes;
    }

    public static Node<NodeData> addNodesToRoot(Map<String, RateLimitConfig> rateLimitConfigs) {
        Map<String, RateLimitConfig> configsWithoutParent = new LinkedHashMap<>(rateLimitConfigs);
        RateLimitConfig rootNodeConfig = configsWithoutParent.remove(ROOT_NODE_NAME);
        NodeData nodeData = rootNodeConfig == null ? null : new NodeData(null, rootNodeConfig);
        rootNode = createNode(ROOT_NODE_NAME, nodeData, null);
        createNode(rootNode, configsWithoutParent);
        return rootNode;
    }

    private static void createNode(Node<NodeData> parent, Map<String, RateLimitConfig> rateLimitConfigs) {
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

    private static Node<NodeData> createNode(String name, NodeData nodeData, Node<NodeData> parent) {
        return new NodeImpl<>(name, nodeData, parent);
    }

    public static AnnotationProcessingException newExceptionForRequiredValue() {
        return new AnnotationProcessingException("Only the root node may have no value");
    }
}
