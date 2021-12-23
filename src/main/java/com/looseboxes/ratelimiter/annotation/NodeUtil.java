package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.NodeImpl;
import com.looseboxes.ratelimiter.util.RateConfigList;

import java.util.*;

public final class NodeUtil {

    private static final Object sourceForPropertyNodes = new Object();

    private static final Object sourceForGroupNodes = new Object();

    private NodeUtil(){}

    public static boolean isPropertyNodeData(NodeValue nodeValue) {
        return nodeValue.getSource() == sourceForPropertyNodes;
    }

    public static void createNodes(Node<NodeValue<RateConfigList>> parent, Map<String, RateConfigList> rateLimitConfigs) {
        Set<Map.Entry<String, RateConfigList>> entrySet = rateLimitConfigs.entrySet();
        for (Map.Entry<String, RateConfigList> entry : entrySet) {
            String name = entry.getKey();
            if(name.equals(parent.getName())) {
                throw new IllegalStateException("Parent and child nodes both have the same name: " + name);
            }
            createNode(parent, name, sourceForPropertyNodes, entry.getValue());
        }
    }

    public static Node<NodeValue<RateConfigList>> createGroupNode(Node<NodeValue<RateConfigList>> parent, String name, RateConfigList rateConfigList) {
        return createNode(parent, name, sourceForGroupNodes, rateConfigList);
    }

    public static Node<NodeValue<RateConfigList>> createNode(
            Node<NodeValue<RateConfigList>> parent, String name,
            Object source, RateConfigList rateConfigList) {
        NodeValue nodeValue = new NodeValue(source, rateConfigList);
        return createNode(name, nodeValue, parent);
    }

    public static Node<NodeValue<RateConfigList>> createNode(String name) {
        return createNode(name, null, null);
    }

    public static Node<NodeValue<RateConfigList>> createNode(String name, NodeValue nodeValue, Node<NodeValue<RateConfigList>> parent) {
        return new NodeImpl<>(name, nodeValue, parent);
    }

    public static AnnotationProcessingException newExceptionForRequiredValue() {
        return new AnnotationProcessingException("Only the root node may have no value");
    }
}
