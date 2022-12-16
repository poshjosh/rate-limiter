package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.annotation.NodeData;
import com.looseboxes.ratelimiter.builder.RateLimitersBuilder;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.formatters.NodeFormatters;
import com.looseboxes.ratelimiter.util.DefaultClassesInPackageFinder;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

class AnnotationProcessingPerformanceIT extends AbstractPerformanceTest{

    @Test
    void annotationProcessShouldConsumeLimitedTimeAndMemory() {
        Usage bookmark = Usage.bookmark();
        Node<NodeData<RateLimiter<Object>>> rateLimiterRootNode = buildRateLimiters();
        assertUsageSinceBookmarkIsLessThan(bookmark, Usage.of(250, 25_000_000));
        System.out.println(NodeFormatters.indentedHeirarchy().format(rateLimiterRootNode));
    }

    Node<NodeData<RateLimiter<Object>>> buildRateLimiters() {
        List<Class<?>> classList = new DefaultClassesInPackageFinder().findClasses(
                Collections.singletonList(getClass().getPackage().getName()),
                clazz -> true);
        return RateLimitersBuilder.tree().build(classList);
    }
}
