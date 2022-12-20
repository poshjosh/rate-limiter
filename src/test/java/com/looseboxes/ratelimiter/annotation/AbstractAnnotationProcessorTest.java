package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.util.ClassesInPackageFinder;
import com.looseboxes.ratelimiter.util.CompositeRate;
import com.looseboxes.ratelimiter.node.Node;

import java.lang.reflect.GenericDeclaration;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public abstract class AbstractAnnotationProcessorTest<S extends GenericDeclaration> {

    List<Class<?>> findClasses() {
        String packageName = this.getClass().getPackage().getName();
        return ClassesInPackageFinder.newInstance().findClasses(packageName, clazz -> true);
    }

    void assertHasChildrenHavingNames(Node<NodeData<CompositeRate>> parent, S... classes) {
        assertHasChildrenHavingNames(parent, toNames(classes));
    }

    void assertHasChildrenHavingNames(Node<NodeData<CompositeRate>> parent, String... names) {
        parent.getChildren().stream().filter(node -> acceptNodeNames(node, names)).findFirst();
    }

    boolean acceptNodeNames(Node<NodeData<CompositeRate>> node, S... classes) {
        return acceptNodeNames(node, toNames(classes));
    }

    String [] toNames(S... classes) {
        return Arrays.stream(classes).map(clazz -> getId(clazz)).collect(Collectors.toList()).toArray(new String[0]);
    }

    boolean acceptNodeNames(Node<NodeData<CompositeRate>> node, String... names) {
        for(String name : names) {
            if(name.equals(node.getName())) {
                return true;
            }
        }
        return false;
    }

    abstract String getId(S element);
}
