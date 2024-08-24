package io.github.poshjosh.ratelimiter.util;

import io.github.poshjosh.ratelimiter.matcher.Matcher;
import io.github.poshjosh.ratelimiter.matcher.Matchers;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class MatchersTest {

    @BeforeAll
    static void setup() {
        System.setProperty("test.property", "test.property.value");
    }

    @AfterAll
    static void teardown() {
        System.clearProperty("test.property");
    }

    @ParameterizedTest
    @CsvSource({
        "jvm.memory.available < 1, false",
        "jvm.memory.available < 999GB, true",
        "jvm.memory.available < 999GB & sys.time.elapsed >= PT0S, true",
        "jvm.memory.available < 999GB & sys.time.elapsed > PT1H, false",
        "jvm.thread.count.deadlocked > 30, false",
        "sys.property[test.property] = test.property.value, true"
    })
    void matches(String expression, boolean expected) {
        Matcher<Object> matcher = Matchers.ofExpression(expression);
        assertEquals(expected, matcher.matches(System.currentTimeMillis()));
    }
}
