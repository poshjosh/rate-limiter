package com.looseboxes.ratelimiter.node;

import com.looseboxes.ratelimiter.util.RateLimitConfig;

import java.io.Serializable;
import java.util.Objects;

public final class NodeData implements Serializable {

    private final Object source;
    private final RateLimitConfig config;

    public NodeData(Object source, RateLimitConfig config) {
        this.source = Objects.requireNonNull(source);
        this.config = Objects.requireNonNull(config);
    }

    public Object getSource() {
        return source;
    }

    public RateLimitConfig getConfig() {
        return config;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        NodeData nodeData = (NodeData) o;
        return source.equals(nodeData.source) && config.equals(nodeData.config);
    }

    @Override
    public int hashCode() {
        return Objects.hash(source, config);
    }
}
