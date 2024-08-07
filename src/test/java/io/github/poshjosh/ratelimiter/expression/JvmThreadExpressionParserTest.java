package io.github.poshjosh.ratelimiter.expression;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ArgumentsSource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.*;

class JvmThreadExpressionParserTest {

    @ParameterizedTest
    @ValueSource(strings = {
            JvmThreadExpressionParser.COUNT+" = ",
            JvmThreadExpressionParser.CURRENT_STATE+" = ",
    })
    void shouldSupport(String expression) {
        assertTrue(ExpressionParsers.ofJvmThread().isSupported(expression));
    }

    @Test
    void shouldNotSupport() {
        assertFalse(ExpressionParsers.ofJvmThread().isSupported("invalid = "));
    }

    @ParameterizedTest
    @ValueSource(strings = {
            JvmThreadExpressionParser.COUNT + " > 0",
            JvmThreadExpressionParser.COUNT_DAEMON + " >= 0",
            JvmThreadExpressionParser.COUNT_DEADLOCKED + " >= 0",
            JvmThreadExpressionParser.COUNT_DEADLOCKED_MONITOR + " >= 0",
            JvmThreadExpressionParser.COUNT_PEAK + " >= 0",
            JvmThreadExpressionParser.COUNT_STARTED + " >= 0",
            JvmThreadExpressionParser.CURRENT_COUNT_BLOCKED + " >= 0",
            JvmThreadExpressionParser.CURRENT_COUNT_WAITED + " >= 0",
            JvmThreadExpressionParser.CURRENT_STATE + " = NEW",
            JvmThreadExpressionParser.CURRENT_SUSPENDED + " = false",
            JvmThreadExpressionParser.CURRENT_TIME_BLOCKED + " > PT0S",
            JvmThreadExpressionParser.CURRENT_TIME_CPU + " > PT0S",
            JvmThreadExpressionParser.CURRENT_TIME_USER + " > PT0S",
            JvmThreadExpressionParser.CURRENT_TIME_WAITED + " > PT0S",
    })
    void shouldSucceed_givenValidExpression(String value) {
        ExpressionParsers.ofJvmThread().parse("", Expressions.of(value));
    }

    @ParameterizedTest
    @ArgumentsSource(InvalidExpressionArgumentsProvider.class)
    void shouldFail_givenInvalidExpression(String value) {
      assertThrows(RuntimeException.class, () -> 
              ExpressionParsers.ofJvmThread().parse("", Expressions.of(value)));
    }
}