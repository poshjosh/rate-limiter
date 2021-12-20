package com.looseboxes.ratelimiter.annotation;

import java.io.Serializable;
import java.util.Objects;

public final class NodeValue<V> implements Serializable {

    private final Object source;
    private final V value;

    public NodeValue(Object source, V value) {
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
        NodeValue nodeValue = (NodeValue) o;
        return source.equals(nodeValue.source) && value.equals(nodeValue.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(source, value);
    }

    @Override public String toString() {
        return "NodeValue{" + "source=" + source.getClass().getSimpleName() + ", value=" + value + '}';
    }
}
