package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.RateLimiterFactory;
import com.looseboxes.ratelimiter.annotation.AnnotationProcessor;
import com.looseboxes.ratelimiter.annotation.NodeValue;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.formatters.NodeFormatters;
import com.looseboxes.ratelimiter.util.ClassesInPackageFinder;
import com.looseboxes.ratelimiter.util.Rates;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;
import java.util.function.BiFunction;

class AnnotationProcessingPerformanceIT {

    @Test
    void annotationProcessShouldConsumeLimitedTimeAndMemory() {
        Usage bookmark = Usage.bookmark();
        Node<NodeValue<RateLimiter<Object>>> rateLimiterRootNode = buildRateLimiters();
        bookmark.assertUsageLessThan(Usage.of(250, 25_000_000));
        System.out.println(NodeFormatters.indentedHeirarchy().format(rateLimiterRootNode));
    }

    Node<NodeValue<RateLimiter<Object>>> buildRateLimiters() {
        List<Class<?>> classList = ClassesInPackageFinder.of().findClasses(
                Collections.singletonList(getClass().getPackage().getName()),
                clazz -> true);
        Node<NodeValue<Rates>> rootNode = Node.of("root");
        AnnotationProcessor.ofRates().processAll(rootNode, classList);

        BiFunction<String, NodeValue<Rates>, NodeValue<RateLimiter<Object>>> valueConverter =
                (nodeName, nodeValue) -> nodeValue.withValue(RateLimiterFactory.of().createNew(nodeValue.getValue()));

        return rootNode.transform(valueConverter);
    }
}
