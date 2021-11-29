package com.looseboxes.ratelimiter.node.formatters;

import com.looseboxes.ratelimiter.util.Experimental;

@Experimental
public final class NodeFormatters {

    private NodeFormatters(){}

    @Experimental
    public static NodeFormatter indentedHeirarchy() {
        return new IndentedHeirarchy();
    }

    @Experimental
    public static NodeFormatter indentedHeirarchy(String indent, int maxLevelsToPrint) {
        return new IndentedHeirarchy(indent, maxLevelsToPrint);
    }

    @Experimental
    public static NodeFormatter indented() {
        return new Indented();
    }

    @Experimental
    public static NodeFormatter indented(String indent) {
        return new Indented(indent);
    }
}
