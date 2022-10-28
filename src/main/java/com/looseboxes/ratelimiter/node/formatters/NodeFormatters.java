package com.looseboxes.ratelimiter.node.formatters;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.Experimental;

import java.util.List;
import java.util.Objects;

@Experimental
public final class NodeFormatters {

    private static class Indented implements NodeFormatter{

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

    private static class IndentedHeirarchy implements NodeFormatter{

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

    private static final Indented indented = new Indented();

    @Experimental
    public static NodeFormatter indented() {
        return indented;
    }

    @Experimental
    public static NodeFormatter indented(String indent) {
        return new Indented(indent);
    }

    private static final NodeFormatter indentedHierarchy = new IndentedHeirarchy();

    @Experimental
    public static NodeFormatter indentedHeirarchy() { return indentedHierarchy; }

    @Experimental
    public static NodeFormatter indentedHeirarchy(String indent, int maxLevelsToPrint) {
        return new IndentedHeirarchy(indent, maxLevelsToPrint);
    }

    private NodeFormatters(){}
}
