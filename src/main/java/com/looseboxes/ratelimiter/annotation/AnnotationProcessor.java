package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.Limit;
import com.looseboxes.ratelimiter.node.Node;

import java.lang.reflect.GenericDeclaration;
import java.util.List;
import java.util.function.BiConsumer;

public interface AnnotationProcessor<S extends GenericDeclaration> {

    default void process(Node<NodeData<Limit>> root, List<S> elements) {
        process(root, elements, (element, node) -> {});
    }

    void process(Node<NodeData<Limit>> root, List<S> elements, BiConsumer<Object, Node<NodeData<Limit>>> consumer);
}
