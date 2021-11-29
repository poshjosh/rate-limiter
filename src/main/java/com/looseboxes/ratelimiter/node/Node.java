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

import com.looseboxes.ratelimiter.util.Experimental;

import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;

/**
 * @author Chinomso Bassey Ikwuagwu on Oct 13, 2017 2:58:54 PM
 * @param <V> The type of the value returned by this node
 */
public interface Node<V> {

    /**
     * Detach this node from its parent
     *
     * @return {code true} if this node had a parent in the first instance, otherwise return {code false}
     * @deprecated Simply put - Do not use. Nodes are not intended to be mutable. This method will
     * thus be removed in the future.
     */
    @Deprecated
    boolean detach();

    /**
     * Copy this node and it's children onto the specified parent.
     *
     * The value of this node is not deep copied. To achieve deep copy use the transform method. Use that method's
     * value converter to create a deep copy of the node's value.
     *
     * @param parent The parent to copy this node and it's children to
     * @return The copy version of this node
     * @see #transform(Node, BiFunction, BiFunction)
     */
    default Node<V> copyTo(Node<V> parent) {
        return transform(parent, (name, value) -> name, (name, value) -> value);
    }

    /**
     * Copy a transformed version of this node and it's children onto the specified parent.
     *
     * @param newParent The parent to copy a transformed version of this node and it's children to
     * @param nameConverter The converter which will be applied to produce a new name for each node in this tree
     * @param valueConverter The converter which will be applied to produce a new value for each node in this tree
     * @param <T> The type of the value of the transformed copy
     * @return The transformed copy of this node
     */
    <T> Node<T> transform(Node<T> newParent, BiFunction<String, V, String> nameConverter, BiFunction<String, V, T> valueConverter);

    default boolean isRoot() {
        return getParentOrDefault(null) == null;
    }

    default boolean isLeaf() {
        return getChildren().isEmpty();
    }
        
    /**
     * The root node is the only node at level <code>Zero (0)</code>
     * The direct children of the root node are at level <code>One (0)</code>
     * and so on and forth
     * @return The level of this node
     * @see #getRoot() 
     */
    default int getLevel() {
        final Node<V> parent = this.getParentOrDefault(null);
        if (parent != null) {
            return parent.getLevel() + 1;
        }else{
            return 0;
        }        
    }

    /**
     * @return The topmost <tt>parent node</tt> in this node's heirarchy.
     */
    default Node<V> getRoot() {
        Node<V> target = this;
        while(target.getParentOrDefault(null) != null) {
            target = (Node)target.getParentOrDefault(null);
        }
        return target;
    }
    
    default Optional<Node<V>> findFirstChild(V... path) {
        
        return this.findFirst(this, path);
    }
    
    Optional<Node<V>> findFirst(Node<V> offset, V... path);

    default Optional<Node<V>> findFirstChild() {
        return findFirstChild(node -> true);
    }

    default Optional<Node<V>> findFirstChild(Predicate<Node<V>> nodeTest) {
        
        return this.findFirst(this, nodeTest);
    }

    Optional<Node<V>> findFirst(Node<V> offset, Predicate<Node<V>> nodeTest);

    Node<V> getChild(int index);

    /**
     * @return An <b>un-modifiable</b> list view of this node's children
     */
    List<Node<V>> getChildren();

    String getName();
    
    default Optional<V> getValueOptional() {
        return Optional.ofNullable(this.getValueOrDefault(null));
    }
    
    V getValueOrDefault(V outpufIfNone);

    default Optional<Node<V>> getParentOptional() {
        return Optional.ofNullable(this.getParentOrDefault(null));
    }
    
    Node<V> getParentOrDefault(Node<V> outputIfNone);
}
