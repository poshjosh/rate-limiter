package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.util.CompositeRate;
import com.looseboxes.ratelimiter.node.Node;

import java.lang.reflect.GenericDeclaration;
import java.util.List;
import java.util.function.BiConsumer;

public interface AnnotationTreeBuilder<S extends GenericDeclaration> {

    default void build(Node<NodeData<CompositeRate>> root, List<S> elements) {
        build(root, elements, (element, node) -> {});
    }

    void build(Node<NodeData<CompositeRate>> root, List<S> elements, BiConsumer<Object, Node<NodeData<CompositeRate>>> consumer);
}
