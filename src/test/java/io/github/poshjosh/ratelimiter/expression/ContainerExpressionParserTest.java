package io.github.poshjosh.ratelimiter.expression;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.*;

class ContainerExpressionParserTest {
    final ExpressionParser<Object, Object> expressionParser = ExpressionParsers.ofContainer();

    @Test
    void shouldSupportSystemTimeIn() {
        assertTrue(expressionParser.isSupported(SystemTimeExpressionParser.TIME+" in "));
    }

    @Test
    void shouldSupportJvmThreadIn() {
        assertTrue(expressionParser.isSupported(JvmThreadExpressionParser.COUNT+" in "));
    }

    @Test
    void shouldNotSupportEquals() {
        assertFalse(expressionParser.isSupported(SystemTimeExpressionParser.TIME + " = "));
    }

    // Do not modify these, they are used by the following test
    public static final Thread.State [] THREAD_STATES = Thread.State.values();
    public static final Thread.State [] BAD_THREAD_STATES = {
            Thread.State.BLOCKED, Thread.State.TERMINATED };

    @ParameterizedTest
    @ValueSource(strings = {
            JvmThreadExpressionParser.CURRENT_STATE+" in io.github.poshjosh.ratelimiter.expression.ContainerExpressionParserTest#THREAD_STATES",
            JvmThreadExpressionParser.CURRENT_STATE+" !in io.github.poshjosh.ratelimiter.expression.ContainerExpressionParserTest#BAD_THREAD_STATES",
            "A in [A, B, C]"
    })
    void shouldSucceed_givenValidExpression(String value) {
        expressionParser.parse(this, Expressions.of(value));
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "A in io.github.poshjosh.ratelimiter.expression.ContainerExpressionParserTest#THREAD_STATES",
            "A in B",
            SystemTimeElapsedExpressionParser.TIME_ELAPSED + " > PT1S"
    })
    void shouldFail_givenInvalidExpression(String value) {
        assertThrows(RuntimeException.class, () ->
                ExpressionParsers.ofSystemTime().parse(this, Expressions.of(value)));
    }
}
