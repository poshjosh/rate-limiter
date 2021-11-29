package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.NodeData;
import com.looseboxes.ratelimiter.node.NodeUtil;

import java.lang.reflect.Method;
import java.util.*;
import java.util.function.BiConsumer;

public class DefaultAnnotationProcessor implements AnnotationProcessor<Class<?>>{

    private final AnnotationProcessor<Class<?>> classAnnotationProcessor;
    private final AnnotationProcessor<Method> methodAnnotationProcessor;

    public DefaultAnnotationProcessor() {
        this(new ClassAnnotationProcessor(), new MethodAnnotationProcessor());
    }

    public DefaultAnnotationProcessor(AnnotationProcessor<Class<?>> classAnnotationProcessor, AnnotationProcessor<Method> methodAnnotationProcessor) {
        this.classAnnotationProcessor = classAnnotationProcessor;
        this.methodAnnotationProcessor = methodAnnotationProcessor;
    }

    @Override
    public Node<NodeData> process(List<Class<?>> classes) {
        Node<NodeData> root = NodeUtil.getOrCreateRootNode();
        process(root, classes);
        return root;
    }

    @Override
    public void process(Node<NodeData> parent, List<Class<?>> classes, BiConsumer<Object, Node<NodeData>> consumer) {

        if(!classes.isEmpty()) {

            BiConsumer<Object, Node<NodeData>> methodProcessor = (element, node) -> {
                if(element instanceof Class) {
                    Method[] methods = ((Class<?>)element).getDeclaredMethods();
                    methodAnnotationProcessor.process(parent, Arrays.asList(methods), consumer);
                }
            };

            classAnnotationProcessor.process(parent, classes, methodProcessor.andThen(consumer));
        }
    }
}
