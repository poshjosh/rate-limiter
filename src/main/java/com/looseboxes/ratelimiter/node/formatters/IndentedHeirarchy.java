package com.looseboxes.ratelimiter.node.formatters;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.Experimental;

import java.util.List;
import java.util.Objects;

@Experimental
public final class IndentedHeirarchy implements NodeFormatter{

    private final String indent;

    private final int maxLevelsToPrint;

    public IndentedHeirarchy() {
        this("  ", Integer.MAX_VALUE);
    }

    public IndentedHeirarchy(String indent, int maxLevelsToPrint) {
        this.indent = Objects.requireNonNull(indent);
        this.maxLevelsToPrint = maxLevelsToPrint;
    }

    @Override
    public <V> StringBuilder appendTo(Node<V> node, StringBuilder appendTo) {
        return appendTo(node, appendTo, maxLevelsToPrint);
    }

    private <V> StringBuilder appendTo(Node<V> node, StringBuilder appendTo, int levelsRemaining) {
        this.doAppendTo(node, appendTo);
        if(levelsRemaining > 0) {
            final List<Node<V>> children = node.getChildren();
            // System.out.println("Levels left: " + levelsRemaining + ", " + node);
            for(Node<V> child : children) {
                this.appendTo(child, appendTo, levelsRemaining - 1);
            }
        }
        return appendTo;
    }

    private <V> void doAppendTo(Node<V> node, StringBuilder appendTo) {
        if(node == null) {
            appendTo.append(node);
            return;
        }
        appendTo.append('\n');
        final int nodeLevel = node.getLevel();
        for(int i=nodeLevel; i>=0; i--) {
            appendTo.append(indent);
        }
        appendTo.append(node.getName()).append('=').append(node.getValueOrDefault(null));
    }
}

