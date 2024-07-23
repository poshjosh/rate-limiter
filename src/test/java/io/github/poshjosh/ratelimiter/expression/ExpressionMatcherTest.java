package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.Matcher;
import org.junit.jupiter.api.Test;

import java.util.UUID;
import java.util.function.Supplier;

import static org.junit.jupiter.api.Assertions.*;

class ExpressionMatcherTest {

    @Test void matchNone_shouldNotMatchValidExpression() {
        Matcher<Object> matcher = ExpressionMatcher.matchNone()
                .matcher("sys.time.elapsed>PT1S")
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertFalse(matcher.matches(0));
        assertFalse(matcher.matches(1000));
        assertFalse(matcher.matches(System.currentTimeMillis()));
    }

    @Test void matchNone_shouldNotMatchInvalidExpression() {
        Matcher<Object> matcher = ExpressionMatcher.matchNone()
                .matcher("1=")
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertFalse(matcher.matches(0));
        assertFalse(matcher.matches(1000));
        assertFalse(matcher.matches(System.currentTimeMillis()));
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
        Matcher<Object> matcher = ExpressionMatcher.ofSystemTimeElapsed()
                .matcher("sys.time.elapsed>PT1H")
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertFalse(matcher.matches(System.currentTimeMillis()));
    }

    @Test void ofSystemTimeElapsed_compositeOr() {
        Matcher<Object> matcher = ExpressionMatcher.ofSystemTimeElapsed()
                .matcher("sys.time.elapsed>=PT0S|sys.time.elapsed<PT1M")
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertTrue(matcher.matches(System.currentTimeMillis()));
    }

    @Test void ofSystemTimeElapsed_compositeAnd() {
        Matcher<Object> matcher = ExpressionMatcher.ofSystemTimeElapsed()
                .matcher("sys.time.elapsed>=PT0S&sys.time.elapsed<PT1M")
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertTrue(matcher.matches(System.currentTimeMillis()));
    }

    @Test void ofSystemProperty() {
        String name0 = UUID.randomUUID().toString() + "0";
        String name1 = UUID.randomUUID().toString() + "1";
        Matcher<Object> matcher = ExpressionMatcher.ofSystemProperty()
                .matcher("sys.property={user.name=" + name0 +
                        "}|sys.property={user.name=" + name1 + "}")
                .orElseThrow(matcherCreationShouldBeSuccessful());
    }

    @Test void ofSystemEnvironment() {
    }

    @Test void ofJvmThread_matchingId() {
        final long currentThreadId = Thread.currentThread().getId();
        Matcher<Object> matcher = ExpressionMatcher.ofJvmThread()
                .matcher("jvm.thread.current.id=" + currentThreadId)
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertTrue(matcher.matches(currentThreadId));
    }

    @Test void ofJvmThread_notMatchingId() {
        Matcher<Object> matcher = ExpressionMatcher.ofJvmThread()
                .matcher("jvm.thread.current.id=111")
                .orElseThrow(matcherCreationShouldBeSuccessful());
        assertFalse(matcher.matches(222));
    }

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