package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.Nullable;
import com.looseboxes.ratelimiter.util.RateConfigList;

import java.lang.reflect.Method;
import java.util.*;
import java.util.function.BiConsumer;

public class ClassAnnotationProcessor extends AbstractAnnotationProcessor<Class<?>> {

    private final AnnotationProcessor<Method> methodAnnotationProcessor;

    public ClassAnnotationProcessor() {
        this(new ClassNameProvider());
    }

    public ClassAnnotationProcessor(IdProvider<Class<?>, String> idProvider) {
        this(idProvider, new MethodAnnotationProcessor());
    }

    public ClassAnnotationProcessor(
            IdProvider<Class<?>, String> idProvider, AnnotationProcessor<Method> methodAnnotationProcessor) {
        super(idProvider);
        this.methodAnnotationProcessor = Objects.requireNonNull(methodAnnotationProcessor);
    }

    // We override this here so we can process the class and its super classes
    @Override
    protected Node<NodeValue<RateConfigList>> process(@Nullable Node<NodeValue<RateConfigList>> root, Class<?> element,
                                                       BiConsumer<Object, Node<NodeValue<RateConfigList>>> consumer){
        Node<NodeValue<RateConfigList>> classNode = null;
        List<Class<?>> superClasses = new ArrayList<>();
        List<Node<NodeValue<RateConfigList>>> superClassNodes = new ArrayList<>();
        BiConsumer<Object, Node<NodeValue<RateConfigList>>> collectSuperClassNodes = (source, superClassNode) -> {
            if(superClasses.contains(source)) {
                superClassNodes.add(superClassNode);
            }
        };
        do{

            Node<NodeValue<RateConfigList>> node = super.process(root, element, collectSuperClassNodes.andThen(consumer));

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

    private void processMethods(@Nullable Node<NodeValue<RateConfigList>> root, Class<?> element, BiConsumer<Object, Node<NodeValue<RateConfigList>>> consumer) {
        Method[] methods = element.getDeclaredMethods();
        methodAnnotationProcessor.process(root, Arrays.asList(methods), consumer);
    }

    /**
     * If class A has 2 super classes B and C both containing resource api endpoint methods, then we transfer
     * those resource api endpoint methods from classes B and C to A.
     * @param classNode The receiving class
     * @param superClassNodes The giving class
     */
    private void transferMethodNodesFromSuperClassNodes(Node<NodeValue<RateConfigList>> classNode, List<Node<NodeValue<RateConfigList>>> superClassNodes) {
        if(classNode != null && !superClassNodes.isEmpty()) {

            for(Node<NodeValue<RateConfigList>> superClassNode : superClassNodes) {

                List<Node<NodeValue<RateConfigList>>> superClassMethodNodes = superClassNode.getChildren();

                // Transfer method nodes from the super class
                superClassMethodNodes.forEach(node -> node.copyTo(classNode));
            }
        }
    }

    @Override
    protected Node<NodeValue<RateConfigList>> getOrCreateParent(@Nullable Node<NodeValue<RateConfigList>> root, Class<?> element,
                                                RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        Node<NodeValue<RateConfigList>> node = findOrCreateNodeForRateLimitGroupOrNull(root, root, element, rateLimitGroup, rateLimits);
        return node == null ? root : node;
    }
}
