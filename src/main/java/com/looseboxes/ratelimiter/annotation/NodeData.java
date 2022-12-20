package com.looseboxes.ratelimiter.annotation;

import java.io.Serializable;
import java.util.Objects;

public final class NodeData<V> implements Serializable {

    private static final long serialVersionUID = 9081726354000000040L;

    private final Object source;
    private final V value;

    public NodeData(Object source, V value) {
        this.source = Objects.requireNonNull(source);
        this.value = Objects.requireNonNull(value);
    }

    public Object getSource() {
        return source;
    }

    public V getValue() {
        return value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        NodeData nodeData = (NodeData) o;
        return source.equals(nodeData.source) && value.equals(nodeData.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(source, value);
    }

    @Override public String toString() {
        return "NodeData{" + "source=" + source.getClass().getSimpleName() + ", value=" + value + '}';
    }
}
