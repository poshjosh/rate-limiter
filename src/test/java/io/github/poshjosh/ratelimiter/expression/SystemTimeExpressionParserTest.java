package io.github.poshjosh.ratelimiter.expression;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ArgumentsSource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.*;

class SystemTimeExpressionParserTest {

    @Test
    void shouldSupport() {
        assertTrue(ExpressionParsers.ofSystemTime().isSupported(
                SystemTimeExpressionParser.TIME+" = "));
    }

    @Test
    void shouldNotSupport() {
        assertFalse(ExpressionParsers.ofSystemTime().isSupported("sys.memory = "));
    }

    @ParameterizedTest
    @ValueSource(strings = {
            SystemTimeExpressionParser.TIME+" = 2023-01-18T20:14:32.846",
            SystemTimeExpressionParser.TIME+" < 2023-01-18T20:14:32",
            SystemTimeExpressionParser.TIME+" <= 2023-01-18T20:14",
    })
    void shouldSucceed_givenValidExpression(String value) {
        ExpressionParsers.ofSystemTime().parse(this, Expressions.of(value));
    }

    @ParameterizedTest
    @ArgumentsSource(InvalidExpressionArgumentsProvider.class)
    void shouldFail_givenInvalidExpression(String value) {
      assertThrows(RuntimeException.class, () -> 
              ExpressionParsers.ofSystemTime().parse(this, Expressions.of(value)));
    }
}