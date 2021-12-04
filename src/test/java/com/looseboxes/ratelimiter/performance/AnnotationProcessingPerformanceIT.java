package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.RateLimiterBuilder;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.formatters.NodeFormatters;
import com.looseboxes.ratelimiter.util.ClassesInPackageFinderImpl;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

public class AnnotationProcessingPerformanceIT extends AbstractPerformanceTest{

    @Test
    void annotationProcessShouldConsumeLimitedTimeAndMemory() {
        recordCurrentTimeAndMemory();
        Node<RateLimiter<Object>> rateLimiterRootNode = buildRateLimiters();
        assertTimeSinceLastRecordIsLessThan(250);
        assertMemorySinceLastRecordIsLessThan(25_000_000);
        System.out.println(NodeFormatters.indentedHeirarchy().format(rateLimiterRootNode));
    }

    Node<RateLimiter<Object>> buildRateLimiters() {
        List<Class<?>> classList = new ClassesInPackageFinderImpl().findClasses(
                Collections.singletonList(getClass().getPackage().getName()),
                clazz -> true);
        return new RateLimiterBuilder().build(classList);
    }
}
