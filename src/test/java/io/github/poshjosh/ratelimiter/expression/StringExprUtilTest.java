package io.github.poshjosh.ratelimiter.expression;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;

class StringExprUtilTest {
    public static String GREETING = "Hello"; // Used by test below, do not modify
    public static String getGreeting() { // Used by test below, do not modify
        return GREETING;
    } // Used by test below, do not modify
    @ParameterizedTest
    @CsvSource({
            "io.github.poshjosh.ratelimiter.expression.StringExprUtilTest#GREETING,Hello",
            "io.github.poshjosh.ratelimiter.expression.StringExprUtilTest#getGreeting(),Hello",
    })
    void tryValueFromJavaType_shouldSucceed(String text, String expected) {
        final String actual = StringExprUtil.tryValueFromJavaType(text, String.class);
        assertEquals(expected, actual,
                "\nExpected: " + expected +
                        "\n  Actual: " + actual + "\n");
    }

    private static Integer getNumber() { // Used by test below, do not modify
        return 9;
    } // Used by test below, do not modify
    @ParameterizedTest
    @CsvSource({
            "NonExistentClass#",
            "io.github.poshjosh.ratelimiter.expression.StringExprUtilTest#getNumber()",
    })
    void tryValueFromJavaType_shouldFail(String text) {
        assertThrows(ExpressionParseException.class,
                () -> StringExprUtil.tryValueFromJavaType(text, Integer.class),
                "\nShould throw IllegalArgumentException, but did not\n");
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "sys.time.elapsed > PT1S | sys.time.elapsed > PT0S,sys.time.elapsed > PT1S,|,sys.time.elapsed > PT0S",
            "name_0[key] = value | name_1 = value_1,name_0[key] = value,|,name_1 = value_1",
            "name_0[key] != [value_0 | value_1] | name_1[key] != [value_0 & value_1],name_0[key] != [value_0 | value_1],|,name_1[key] != [value_0 & value_1]"
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