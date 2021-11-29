/*
 * Copyright 2017 NUROX Ltd.
 *
 * Licensed under the NUROX Ltd Software License (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.looseboxes.com/legal/licenses/software.html
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.looseboxes.ratelimiter.node;

import java.util.List;
import java.util.Objects;

/**
 * @author Chinomso Bassey Ikwuagwu on Oct 19, 2017 5:04:09 PM
 */
public class NodeFormatter {
    
    private final String indent;
    
    private final int numberOfLevels;
    
    public NodeFormatter() {
        this("  ", Integer.MAX_VALUE);
    }

    public NodeFormatter(String indent, int numberOfLevels) {
        this.indent = Objects.requireNonNull(indent);
        this.numberOfLevels = numberOfLevels;
    }
    
    public String format(Node node) {
        return this.appendNode(node, new StringBuilder()).toString();
    }

    public StringBuilder appendNode(Node node, StringBuilder appendTo) {
        return this.appendNode(node, appendTo, this.numberOfLevels);
    }
    
    public StringBuilder appendNode(Node node, StringBuilder appendTo, int levelsRemaining) {
        this.doAppendNode(node, appendTo);
        if(levelsRemaining > 0) {
            final List<Node> children = node.getChildren();
//            System.out.println("Levels left: " + levelsRemaining + ", " + node);
            for(Node child : children) {
                this.appendNode(child, appendTo, levelsRemaining - 1);
            }
        }
        return appendTo;
    }

    private void doAppendNode(Node node, StringBuilder appendTo) {
        appendTo.append('\n');
        final int nodeLevel = node.getLevel();
        for(int i=nodeLevel; i>=0; i--) {
            appendTo.append(indent);
        }
        appendTo.append(node.getName());
        final Object nodeValue = node.getValueOrDefault(null);
        if(nodeValue != null) {
            appendTo.append('=').append(nodeValue);
        }
    }
}
