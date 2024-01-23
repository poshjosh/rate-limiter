package io.github.poshjosh.ratelimiter.expression;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;

class StringExprUtilTest {

    @ParameterizedTest
    @CsvSource({
            "web.invalid.uri,=,/abc?key1=val1",
            "jvm.memory.free,=,1_000",
            "jvm.memory.free,!<=,1_000",
            "     \tweb.request.user.role ,  !$,  ROLE_ADMIN  ",
            "sys.environment,=,{service.instances!>=3}",
            "something,=,[a|b]",
            "something,=,{key=[a&b]}"
    })
    void testValidExpressions(String lhs, String operator, String rhs) {
        Expression<String> expression = StringExprUtil.splitIntoExpression(lhs + operator + rhs);
        assertEquals(lhs, expression.getLeftOrDefault(null));
        assertEquals(operator, expression.getOperator().getSymbol());
        assertEquals(rhs, expression.getRightOrDefault(null));
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "lhs>>>rhs",
            "lhs|",
            "1_000"
    })
    void testInValidExpressions(String expression) {
        assertThrows(RuntimeException.class, () -> StringExprUtil.splitIntoExpression(expression));
    }


    @ParameterizedTest
    @ValueSource(strings = {
            "sys.time.elapsed>PT1S|sys.time.elapsed>PT0S,sys.time.elapsed>PT1S,|,sys.time.elapsed>PT0S",
            "name_0={key=value}|name_1=value_1,name_0={key=value},|,name_1=value_1",
            "name_0!={key=[value_0|value_1]}|name_1!={key=[value_0&value_1]},name_0!={key=[value_0|value_1]},|,name_1!={key=[value_0&value_1]}"
    })
    void splitIntoExpressionsAndConjunctors(String s) {
        String [] arr = s.split(",");
        String [] expected = new String[arr.length - 1];
        System.arraycopy(arr, 1, expected, 0, expected.length);
        String [] actual = StringExprUtil.splitIntoExpressionsAndConjunctors(arr[0]);

        assertArrayEquals(expected, actual,
                "\nExpected: " + Arrays.toString(expected) +
                        "\n  Actual: " + Arrays.toString(actual) + "\n");
    }
}