package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.formatters.NodeFormatters;
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
        Node<NodeData> root = new ClassAnnotationProcessor(this::getId).process(classes);
        System.out.println();
        System.out.println(NodeFormatters.indentedHeirarchy().format(root));
        assertThat(root.findFirstChild(node -> node.getName().equals("root")).isPresent()).isTrue();
        assertHasChildrenHavingNames(root, "ClassGroupOnlyAnon", "PrivateClass", "InternalClass");
        assertHasChildrenHavingNames(root, "Fire");
        Node<NodeData> fire = root.findFirstChild(node -> "Fire".equals(node.getName())).orElse(null);
        assertHasChildrenHavingNames(fire,
                ClassWithClassAnnotations.ClassGroupOnlyNamedFire.class,
                ClassWithClassAnnotations.SecondClassGroupOnlyNamedFire.class);
    }

    String getId(Class<?> element) {
        return element.getSimpleName();
    }
}