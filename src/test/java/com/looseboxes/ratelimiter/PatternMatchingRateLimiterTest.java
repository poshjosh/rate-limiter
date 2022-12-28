package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.annotation.NodeValue;
import com.looseboxes.ratelimiter.annotation.RateLimiterFromAnnotationFactory;
import com.looseboxes.ratelimiter.annotations.RateLimit;
import com.looseboxes.ratelimiter.annotations.RateLimitGroup;
import com.looseboxes.ratelimiter.node.BreadthFirstNodeVisitor;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.util.Operator;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static java.util.concurrent.TimeUnit.SECONDS;
import static org.junit.jupiter.api.Assertions.*;

class PatternMatchingRateLimiterTest {

    final Object key = "one";

    @RateLimit(limit = 1, duration = 1, timeUnit = SECONDS)
    static class RateLimitedClass0{ }

    @Test
    void testRateLimitedClass() {
        RateLimiter<Object> rateLimiter = buildRateLimiter(1, RateLimitedClass0.class);
        assertTrue(rateLimiter.tryConsume(key));
        assertFalse(rateLimiter.tryConsume(key));
    }

    static class RateLimitedClass1{
        @RateLimit(limit = 1, duration = 1, timeUnit = SECONDS)
        void rateLimitedClass1_method_0() { }
    }

    @Test
    void testClassWithSingleRateLimitedMethod() {
        RateLimiter<Object> rateLimiter = buildRateLimiter(1, RateLimitedClass1.class);
        assertTrue(rateLimiter.tryConsume(key));
        assertFalse(rateLimiter.tryConsume(key));
    }

    @RateLimit(limit = 1, duration = 1, timeUnit = SECONDS)
    static class RateLimitedClass2{
        @RateLimit(limit = 1, duration = 1, timeUnit = SECONDS)
        void rateLimitedClass2_method_0() { }
    }

    @Test
    void testRateLimitedClassWithSingleRateLimitedMethod() {
        RateLimiter<Object> rateLimiter = buildRateLimiter(2, RateLimitedClass2.class);
        assertTrue(rateLimiter.tryConsume(key));
        assertFalse(rateLimiter.tryConsume(key));
    }

    @RateLimitGroup(operator = Operator.OR)
    @RateLimit(limit = 1, duration = 1, timeUnit = SECONDS)
    @RateLimit(limit = 3, duration = 1, timeUnit = SECONDS)
    static class RateLimitedClass3{ }

    @Test
    void testRateLimitedClassWithOrLimits() {
        RateLimiter<Object> rateLimiter = buildRateLimiter(1, RateLimitedClass3.class);
        assertTrue(rateLimiter.tryConsume(key));
        assertFalse(rateLimiter.tryConsume(key));
    }

    @RateLimitGroup(operator = Operator.AND)
    @RateLimit(limit = 1, duration = 1, timeUnit = SECONDS)
    @RateLimit(limit = 3, duration = 1, timeUnit = SECONDS)
    static class RateLimitedClass4{ }

    @Test
    void testRateLimitedClassWithAndLimits() {
        RateLimiter<Object> rateLimiter = buildRateLimiter(1, RateLimitedClass4.class);
        assertTrue(rateLimiter.tryConsume(key));
        assertTrue(rateLimiter.tryConsume(key));
        assertTrue(rateLimiter.tryConsume(key));
        assertFalse(rateLimiter.tryConsume(key));
    }

    private RateLimiter<Object> buildRateLimiter(int expectedNodes, Class<?>... classes) {

        Node<NodeValue<RateLimiter<Object>>> rateLimiterRootNode = RateLimiterFromAnnotationFactory.of().createNode(classes);
        //System.out.println(NodeFormatters.indentedHeirarchy().format(rateLimiterRootNode));

        assertEquals(expectedNodes, numberOfNodes(rateLimiterRootNode));

        PatternMatchingRateLimiter.MatcherProvider<Object> matcherProvider = (nodeName, nodeValue) -> {
            return key -> nodeName;
        };

        boolean firstMatchOnly = false; // false for annotations, true for properties
        return new PatternMatchingRateLimiter<>(matcherProvider, (Node)rateLimiterRootNode, firstMatchOnly);
    }

    private int numberOfNodes(Node node) {
        final AtomicInteger count = new AtomicInteger();
        new BreadthFirstNodeVisitor<>(currentNode -> count.incrementAndGet()).accept(node);
        return count.decrementAndGet(); // We subtract the root node
    }
}
