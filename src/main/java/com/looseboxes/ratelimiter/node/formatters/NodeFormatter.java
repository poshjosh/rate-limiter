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

package com.looseboxes.ratelimiter.node.formatters;

import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.annotations.Beta;

/**
 * @author Chinomso Bassey Ikwuagwu on Oct 19, 2017 5:04:09 PM
 */
@Beta
public interface NodeFormatter {

    default <V> String format(Node<V> node) {
        return appendTo(node, new StringBuilder()).toString();
    }

    <V> StringBuilder appendTo(Node<V> node, StringBuilder appendTo);
}
