package com.looseboxes.ratelimiter.node.formatters;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.Experimental;

import java.util.Objects;

@Experimental
public class Indented implements NodeFormatter{

    private final String indent;

    public Indented() {
        this("  ");
    }

    public Indented(String indent) {
        this.indent = Objects.requireNonNull(indent);
    }

    @Override
    public <V> StringBuilder appendTo(Node<V> node, StringBuilder appendTo) {
        if(node == null) {
            return appendTo.append((Object)null);
        }
        final int nodeLevel = node.getLevel();
        for(int i=nodeLevel; i>=0; i--) {
            appendTo.append(indent);
        }
        return appendTo.append(node.getName()).append('=').append(node.getValueOrDefault(null));
    }
}
