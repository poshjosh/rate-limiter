package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.Matcher;
import org.junit.jupiter.api.Test;

import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

import static org.junit.jupiter.api.Assertions.*;

class ExpressionMatcherTest {

    @Test void matchNone_shouldNotMatchValidExpression() {
        Matcher<Object> matcher = ExpressionMatchers.matchNone()
                .matcher("sys.time.elapsed > PT1S")
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertFalse(matcher.matches(0));
        assertFalse(matcher.matches(1000));
        assertFalse(matcher.matches(null));
    }

    @Test void matchNone_shouldNotMatchInvalidExpression() {
        Matcher<Object> matcher = ExpressionMatchers.matchNone()
                .matcher("1 = ")
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertFalse(matcher.matches(0));
        assertFalse(matcher.matches(1000));
        assertFalse(matcher.matches(null));
    }

    @Test void ofDefault() {
    }

    @Test void any() {
    }

    @Test void ofSystemMemory() {
    }

    @Test void ofSystemTime() {
    }

    @Test void ofSystemTimeElapsed() {
        Matcher<Object> matcher = ExpressionMatchers.ofSystemTimeElapsed()
                .matcher("sys.time.elapsed > PT1H")
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertFalse(matcher.matches(System.currentTimeMillis()));
    }

    @Test void ofSystemTimeElapsed_givenCustomStartTime() {
        Matcher<Object> matcher = ExpressionMatchers.ofSystemTimeElapsed()
                .matcher("sys.time.elapsed > PT1H")
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertTrue(matcher.matches(System.currentTimeMillis() - TimeUnit.HOURS.toMillis(3)));
    }

    @Test void ofSystemTimeElapsed_givenNullStartTime() {
        Matcher<Object> matcher = ExpressionMatchers.ofSystemTimeElapsed()
                .matcher("sys.time.elapsed > PT0S")
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertTrue(matcher.matches(null));
    }

    @Test void ofSystemTimeElapsed_compositeOr() {
        Matcher<Object> matcher = ExpressionMatchers.ofSystemTimeElapsed()
                .matcher("sys.time.elapsed >= PT0S | sys.time.elapsed < PT1M")
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertTrue(matcher.matches(System.currentTimeMillis()));
    }

    @Test void ofDefaults_compositeAnd() {
        Matcher<Object> matcher = ExpressionMatchers.ofDefaults()
                .matcher("sys.time.elapsed >= PT0S & jvm.thread.count < 1000")
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertTrue(matcher.matches(System.currentTimeMillis()));
    }

    @Test void ofSystemTimeElapsed_compositeAnd() {
        Matcher<Object> matcher = ExpressionMatchers.ofSystemTimeElapsed()
                .matcher("sys.time.elapsed >= PT0S & sys.time.elapsed < PT1M")
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertTrue(matcher.matches(System.currentTimeMillis()));
    }

    @Test void ofSystemProperty() {
        String name0 = UUID.randomUUID().toString() + "0";
        String name1 = UUID.randomUUID().toString() + "1";
        Matcher<Object> matcher = ExpressionMatchers.ofSystemProperty()
                .matcher("sys.property[user.name] = " + name0 +
                        " | sys.property[user.name] = " + name1)
                .orElseThrow(matcherCreationShouldBeSuccessful());
    }

    @Test void ofSystemEnvironment() {
    }

    @Test void ofJvmThread_matchingId() {
        final long currentThreadId = Thread.currentThread().getId();
        Matcher<Object> matcher = ExpressionMatchers.ofJvmThread()
                .matcher("jvm.thread.current.id = " + currentThreadId)
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertTrue(matcher.matches(currentThreadId));
    }

    @Test void ofJvmThread_notMatchingId() {
        Matcher<Object> matcher = ExpressionMatchers.ofJvmThread()
                .matcher("jvm.thread.current.id = 111")
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertFalse(matcher.matches(222));
    }

    @Test void ofContainer() { }


    private Supplier<AssertionError> matcherCreationShouldBeSuccessful() {
        return () -> new AssertionError("Matcher creation should be successful, but was not.");
    }

    @Test void of() {
    }

    @Test void testOf() {
    }

    @Test void match() {
    }

    @Test void matcher() {
    }

    @Test void isSupported() {
    }

    @Test void testMatcher() {
    }
}