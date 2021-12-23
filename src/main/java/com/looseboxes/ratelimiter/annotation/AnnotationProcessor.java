package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.RateConfigList;

import java.lang.reflect.GenericDeclaration;
import java.util.List;
import java.util.function.BiConsumer;

public interface AnnotationProcessor<S extends GenericDeclaration> {

    default void process(Node<NodeValue<RateConfigList>> root, List<S> elements) {
        process(root, elements, (element, node) -> {});
    }

    void process(Node<NodeValue<RateConfigList>> root, List<S> elements, BiConsumer<Object, Node<NodeValue<RateConfigList>>> consumer);
}
