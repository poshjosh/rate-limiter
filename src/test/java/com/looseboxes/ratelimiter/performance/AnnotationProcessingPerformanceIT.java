package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.annotation.NodeData;
import com.looseboxes.ratelimiter.builder.RateLimitersBuilder;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.formatters.NodeFormatters;
import com.looseboxes.ratelimiter.util.ClassesInPackageFinder;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

class AnnotationProcessingPerformanceIT {

    @Test
    void annotationProcessShouldConsumeLimitedTimeAndMemory() {
        Usage bookmark = Usage.bookmark();
        Node<NodeData<RateLimiter<Object>>> rateLimiterRootNode = buildRateLimiters();
        bookmark.assertUsageLessThan(Usage.of(250, 25_000_000));
        System.out.println(NodeFormatters.indentedHeirarchy().format(rateLimiterRootNode));
    }

    Node<NodeData<RateLimiter<Object>>> buildRateLimiters() {
        List<Class<?>> classList = ClassesInPackageFinder.of().findClasses(
                Collections.singletonList(getClass().getPackage().getName()),
                clazz -> true);
        return RateLimitersBuilder.tree().build(classList);
    }
}
