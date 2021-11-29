package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;

import java.lang.reflect.GenericDeclaration;
import java.util.List;
import java.util.function.BiConsumer;

public interface AnnotationProcessor<S extends GenericDeclaration> {

    Node<NodeData> process(List<S> elements);

    default void process(Node<NodeData> parent, List<S> elements) {
        process(parent, elements, (element, node) -> {});
    }

    void process(Node<NodeData> parent, List<S> elements, BiConsumer<Object, Node<NodeData>> consumer);
}
