package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.NodeData;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiConsumer;

public final class ClassAnnotationProcessor extends AnnotationProcessorImpl<Class<?>>{

    public ClassAnnotationProcessor() {
        super(new ClassNameProvider());
    }

    public ClassAnnotationProcessor(IdProvider<Class<?>, String> idProvider) {
        super(idProvider);
    }

    // We override this here so we can process the class and its super classes
    @Override
    protected Node<NodeData> process(Node<NodeData> root, Class<?> element, BiConsumer<Object, Node<NodeData>> consumer){
        Node<NodeData> classNode = null;
        List<Node<NodeData>> superClassNodes = new LinkedList<>();
        AtomicBoolean mainNodeCollected = new AtomicBoolean(false);

        BiConsumer<Object, Node<NodeData>> collectSuperClassNodes = (e, node) -> {
            if(node == null) {
                return;
            }
            if(!mainNodeCollected.get()) {
                mainNodeCollected.set(true);
                return;
            }
            superClassNodes.add(node);
        };

        do{
            Node<NodeData> node = super.process(root, element, collectSuperClassNodes.andThen(consumer));
            if(classNode == null) { // The first successfully processed node is the result
                classNode = node;
            }
            element = element.getSuperclass();
        }while(element != null && !element.equals(Object.class));

        transferMethodNodesFromSuperClassNodes(classNode, superClassNodes);

        return classNode;
    }

    /**
     * If class A has 2 super classes B and C both containing resource api endpoint methods, then we transfer
     * those resource api endpoint methods from classes B and C to A.
     * @param classNode The receiving class
     * @param superClassNodes The giving class
     */
    private void transferMethodNodesFromSuperClassNodes(Node<NodeData> classNode, List<Node<NodeData>> superClassNodes) {
        if(classNode != null && !superClassNodes.isEmpty()) {

            for(Node<NodeData> superClassNode : superClassNodes) {

                List<Node<NodeData>> superClassMethodNodes = superClassNode.getChildren();

                // Transfer method nodes from the super class
                superClassMethodNodes.forEach(node -> node.copyTo(classNode));

                // Detach the transferred method nodes
                superClassMethodNodes.forEach(Node::detach);
            }
        }
    }

    protected Node<NodeData> getOrCreateParent(Node<NodeData> root, Class<?> element,
                                               RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        Node<NodeData> node = findOrCreateNodeForRateLimitGroupOrNull(root, root, rateLimitGroup, rateLimits);
        return node == null ? root : node;
    }
}
