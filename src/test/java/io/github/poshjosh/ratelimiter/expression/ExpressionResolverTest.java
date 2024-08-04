package io.github.poshjosh.ratelimiter.expression;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Arrays;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

class ExpressionResolverTest {

    @ParameterizedTest
    @CsvSource({
            JvmThreadExpressionParser.COUNT + " >= 0,true",
            JvmThreadExpressionParser.COUNT_DAEMON + " >= 0,true",
            JvmThreadExpressionParser.COUNT_DEADLOCKED + " >= 0,true",
            JvmThreadExpressionParser.COUNT_DEADLOCKED_MONITOR + " >= 0,true",
            JvmThreadExpressionParser.COUNT_PEAK + " >= 0,true",
            JvmThreadExpressionParser.COUNT_STARTED + " >= 0,true",
            JvmThreadExpressionParser.CURRENT_COUNT_BLOCKED + " >= 0,true",
            JvmThreadExpressionParser.CURRENT_COUNT_WAITED + " >= 0,true",
            JvmThreadExpressionParser.CURRENT_STATE + " = RUNNABLE,true",
            JvmThreadExpressionParser.CURRENT_STATE + " = BLOCKED,false",
            JvmThreadExpressionParser.CURRENT_SUSPENDED + " = false,true",
            JvmThreadExpressionParser.CURRENT_TIME_BLOCKED + " <= PT0S,true",
            JvmThreadExpressionParser.CURRENT_TIME_CPU + " >= PT0S,true",
            JvmThreadExpressionParser.CURRENT_TIME_USER + " >= PT0S,true",
            JvmThreadExpressionParser.CURRENT_TIME_WAITED + " <= PT0S,true",
    })
    void testJvmThreadExpression(String expressionString, boolean expectedResult) {
        Expression<Object> expression = ExpressionParsers.ofJvmThread()
                .parse(this, Expressions.of(expressionString));
        //System.out.println(expression);
        testExpression(ExpressionResolvers.ofJvmThread(), expression, expectedResult);
    }

    @ParameterizedTest
    @CsvSource({
            "1,=,1,true",
            "1,>,1,false",
            "1,>=,1,true",
            "1,<,1,false",
            "1,<=,1,true"
    })
    void testValidLongExpression(Long lhs, String operator, Long rhs, boolean expectedResult) {
        testExpression(ExpressionResolvers.ofLong(), lhs, operator, rhs, expectedResult);
    }

    @ParameterizedTest
    @CsvSource({
            "1.0,=,1.0,true",
            "1,>,1,false",
            "1,>=,1,true",
            "1,<,1,false",
            "1,<=,1,true"
    })
    void testValidDecimalExpression(Double lhs, String operator, Double rhs, boolean expectedResult) {
        testExpression(ExpressionResolvers.ofDecimal(), lhs, operator, rhs, expectedResult);
    }

    private static final Integer [] DIGITS = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    private static Stream<Arguments> containerExpressionArgsFor(Object digitsContainer) {
        return Stream.of(
                Arguments.of(0, Operator.IN, digitsContainer, true),
                Arguments.of(-1, Operator.IN, digitsContainer, false),
                Arguments.of(10, Operator.IN, digitsContainer, false),
                Arguments.of(1, Operator.IN, "[0, 1, 2]", true),
                Arguments.of(0, Operator.NOT_IN, digitsContainer, false),
                Arguments.of(-1, Operator.NOT_IN, digitsContainer, true),
                Arguments.of(10, Operator.NOT_IN, digitsContainer, true),
                Arguments.of(1, Operator.NOT_IN, "[0, 1, 2]", false)
        );
    }
    private static Stream<Arguments> containerExpressionArgsArray() {
        return containerExpressionArgsFor(DIGITS);
    }
    private static Stream<Arguments> containerExpressionArgsCollection() {
        return containerExpressionArgsFor(Arrays.asList(DIGITS));
    }
    private static Stream<Arguments> containerExpressionArgsString() {
        return containerExpressionArgsFor(Arrays.toString(DIGITS));
    }
    @ParameterizedTest
    @MethodSource("containerExpressionArgsArray")
    void testValidArrayExpression(Integer lhs, Operator operator, Object rhs, boolean expectedResult) {
        testExpression(ExpressionResolvers.ofContainer(), lhs, operator, rhs, expectedResult);
    }
    @ParameterizedTest
    @MethodSource("containerExpressionArgsCollection")
    void testValidCollectionExpression(Integer lhs, Operator operator, Object rhs, boolean expectedResult) {
        testExpression(ExpressionResolvers.ofContainer(), lhs, operator, rhs, expectedResult);
    }
    @ParameterizedTest
    @MethodSource("containerExpressionArgsString")
    void testValidStringContainerExpression(Integer lhs, Operator operator, Object rhs, boolean expectedResult) {
        testExpression(ExpressionResolvers.ofContainer(), lhs, operator, rhs, expectedResult);
    }

    void testExpression(ExpressionResolver<?> resolver,
            Object l, String operator, Object r, boolean expectedResult) {
        testExpression(resolver, l, Operators.ofSymbol(operator), r, expectedResult);
    }

    void testExpression(ExpressionResolver<?> resolver,
            Object l, Operator operator, Object r, boolean expectedResult) {
        Expression<?> expression = Expressions.of(l, operator, r);
        testExpression(resolver, expression, expectedResult);
    }

    void testExpression(ExpressionResolver<?> resolver, Expression expression, boolean expected) {
        boolean result = resolver.resolve(expression);
        assertEquals(expected, result);
        result = resolver.resolve(expression.flipOperator());
        assertNotEquals(expected, result);
    }
}