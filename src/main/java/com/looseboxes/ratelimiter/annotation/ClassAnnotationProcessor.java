package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.Nullable;

import java.lang.reflect.Method;
import java.util.*;
import java.util.function.BiConsumer;

class ClassAnnotationProcessor<T> extends AnnotationProcessor<Class<?>, T> {

    private final AnnotationProcessor<Method, T> methodAnnotationProcessor;

    ClassAnnotationProcessor(
            IdProvider<Class<?>, String> idProvider,
            AnnotationProcessor.Converter<T> converter,
            AnnotationProcessor<Method, T> methodAnnotationProcessor) {
        super(idProvider, converter);
        this.methodAnnotationProcessor = Objects.requireNonNull(methodAnnotationProcessor);
    }

    // We override this here so we can process the class and its super classes
    @Override
    protected Node<NodeData<T>> process(@Nullable Node<NodeData<T>> root, Class<?> element,
                                        BiConsumer<Object, Node<NodeData<T>>> consumer){
        Node<NodeData<T>> classNode = null;
        List<Class<?>> superClasses = new ArrayList<>();
        List<Node<NodeData<T>>> superClassNodes = new ArrayList<>();
        BiConsumer<Object, Node<NodeData<T>>> collectSuperClassNodes = (source, superClassNode) -> {
            if(superClasses.contains(source)) {
                superClassNodes.add(superClassNode);
            }
        };
        do{

            Node<NodeData<T>> node = super.process(root, element, collectSuperClassNodes.andThen(consumer));

            final boolean mainNode = classNode == null;

            // If not main node, then it is a super class node, in which case we do not attach
            // the super class node to the root by passing in null as its parent
            // We will transfer all method nodes from each super class node to the main node
            processMethods(mainNode ? root : null, element, consumer);

            if(mainNode) { // The first successfully processed node is the base class
                classNode = node;
            }else{
                superClasses.add(element);
            }

            element = element.getSuperclass();

        }while(element != null && !element.equals(Object.class));

        transferMethodNodesFromSuperClassNodes(classNode, superClassNodes);

        return classNode;
    }

    private void processMethods(@Nullable Node<NodeData<T>> root, Class<?> element, BiConsumer<Object, Node<NodeData<T>>> consumer) {
        Method[] methods = element.getDeclaredMethods();
        methodAnnotationProcessor.process(root, Arrays.asList(methods), consumer);
    }

    /**
     * If class A has 2 super classes B and C both containing resource api endpoint methods, then we transfer
     * those resource api endpoint methods from classes B and C to A.
     * @param classNode The receiving class
     * @param superClassNodes The giving class
     */
    private void transferMethodNodesFromSuperClassNodes(Node<NodeData<T>> classNode, List<Node<NodeData<T>>> superClassNodes) {
        if(classNode != null && !superClassNodes.isEmpty()) {

            for(Node<NodeData<T>> superClassNode : superClassNodes) {

                List<Node<NodeData<T>>> superClassMethodNodes = superClassNode.getChildren();

                // Transfer method nodes from the super class
                superClassMethodNodes.forEach(node -> node.copyTo(classNode));
            }
        }
    }

    @Override
    protected Node<NodeData<T>> getOrCreateParent(@Nullable Node<NodeData<T>> root, Class<?> element,
                                                  RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        Node<NodeData<T>> node = findOrCreateNodeForRateLimitGroupOrNull(root, root, element, rateLimitGroup, rateLimits);
        return node == null ? root : node;
    }
}
