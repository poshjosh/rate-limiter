package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.formatters.NodeFormatters;
import com.looseboxes.ratelimiter.util.Rates;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

class ClassAnnotationProcessorTest extends AbstractAnnotationProcessorTest<Class<?>> {

    @Test
    public void methodNodesFromSuperClassesShouldBeTransferredToResourceAnnotatedClass() {
        // @TODO
    }

    @Test
    public void classCannotHaveMultipleClassLevelResourceAnnotationsInHeirarchy() {
        // @TODO
    }

    @Test
    public void nodeVisitingShouldBeAccurate() {
        List<Class<?>> classes = findClasses();
//        System.out.println("Found classes: " + classes);
        final String rootNodeName = "sample-root-node";
        Node<NodeData<Rates>> root = NodeUtil.createNode(rootNodeName);
        getInstance().process(root, classes);
        System.out.println();
        System.out.println(NodeFormatters.indentedHeirarchy().format(root));
        assertThat(root.findFirstChild(node -> node.getName().equals(rootNodeName)).isPresent()).isTrue();
        assertHasChildrenHavingNames(root, "ClassGroupOnlyAnon", "PrivateClass", "InternalClass");
        assertHasChildrenHavingNames(root, "Fire");
        Node<NodeData<Rates>> fire = root.findFirstChild(node -> "Fire".equals(node.getName())).orElse(null);
        assertHasChildrenHavingNames(fire,
                ClassWithClassAnnotations.ClassGroupOnlyNamedFire.class,
                ClassWithClassAnnotations.SecondClassGroupOnlyNamedFire.class);
    }

    AnnotationProcessor<Class<?>, Rates> getInstance() {
        return AnnotationProcessor.newInstance(this::getId, IdProvider.forMethod(), new AnnotationToRatesConverter());
    }

    String getId(Class<?> element) {
        return element.getSimpleName();
    }
}