package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.annotation.RateLimit;
import com.looseboxes.ratelimiter.builder.RateLimitersBuilder;
import com.looseboxes.ratelimiter.node.BreadthFirstNodeVisitor;
import com.looseboxes.ratelimiter.node.Node;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

class PatternMatchingRateLimiterTest {

    final Object key = "one";

    static class RateLimitedClass0{
        @RateLimit(limit = 1)
        void rateLimitedClass0_method_limit_1() { }
    }

    @Test
    void testSingleRateLimitedMethod() {
        RateLimiter<Object> rateLimiter = buildRateLimiter(1, RateLimitedClass0.class);
        assertTrue(rateLimiter.consume(key, 1));
        assertFalse(rateLimiter.consume(key, 1));
    }

    @RateLimit(limit = 1)
    static class RateLimitedClass1{
        @RateLimit(limit = 1)
        void rateLimitedClass1_method_limit_1() { }
    }

    @Test
    void testRateLimitedClassWithSingleRateLimitedMethod() {
        RateLimiter<Object> rateLimiter = buildRateLimiter(2, RateLimitedClass1.class);
        assertTrue(rateLimiter.consume(key, 1));
        assertFalse(rateLimiter.consume(key, 1));
    }

    private RateLimiter<Object> buildRateLimiter(int expectedNodes, Class<?>... classes) {
        Node rootNode = RateLimitersBuilder.tree().build(classes);
        //System.out.println(NodeFormatters.indentedHeirarchy().format(rootNode));
        assertEquals(expectedNodes, numberOfNodes(rootNode));
        boolean firstMatchOnly = false; // false for annotations, true for properties
        return new PatternMatchingRateLimiter<Object>(rootNode, firstMatchOnly);
    }

    private int numberOfNodes(Node node) {
        final AtomicInteger count = new AtomicInteger();
        new BreadthFirstNodeVisitor<>(currentNode -> count.incrementAndGet()).accept(node);
        return count.decrementAndGet(); // We subtract the root node
    }
}
