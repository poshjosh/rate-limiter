package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.NodeImpl;

import java.util.*;

public final class NodeUtil {

    private static final Object sourceForGroupNodes = new Object();

    private NodeUtil(){}

    public static <V> boolean isEqual(Node<NodeData<V>> node, String name, NodeData<V> nodeData) {
        return Objects.equals(node.getName(), name) &&
                Objects.equals(node.getValueOrDefault(null), nodeData);
    }

    public static <V> Node<NodeData<V>> createGroupNode(Node<NodeData<V>> parent, String name, V value) {
        return createNode(parent, name, sourceForGroupNodes, value);
    }

    public static <V> Node<NodeData<V>> createNode(
            Node<NodeData<V>> parent, String name,
            Object source, V value) {
        NodeData nodeData = new NodeData(source, value);
        return createNode(name, nodeData, parent);
    }

    public static <V> Node<NodeData<V>> createNode(String name) {
        return createNode(name, null, null);
    }

    public static <V> Node<NodeData<V>> createNode(String name, NodeData nodeData, Node<NodeData<V>> parent) {
        return new NodeImpl<>(name, nodeData, parent);
    }

    public static AnnotationProcessingException newExceptionForRequiredValue() {
        return new AnnotationProcessingException("Only the root node may have no value");
    }
}
