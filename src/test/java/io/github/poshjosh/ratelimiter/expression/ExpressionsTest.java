package io.github.poshjosh.ratelimiter.expression;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ExpressionsTest {
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
        Expression<String> expression = Expressions.of(lhs + operator + rhs);
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
        assertThrows(RuntimeException.class, () -> Expressions.of(expression));
    }
}
