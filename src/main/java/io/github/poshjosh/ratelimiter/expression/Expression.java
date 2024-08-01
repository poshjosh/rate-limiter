package io.github.poshjosh.ratelimiter.expression;

import java.util.Objects;

public interface Expression<OPERAND_TYPE> {
    Expression<Object> TRUE = ofDefault(null, Operator.EQUALS, null);
    Expression<Object> FALSE = TRUE.flipOperator();

    static Expression<String> ofDefault(String expression) {
        return StringExprUtil.toExpression(expression);
    }
    static <T> Expression<T> ofDefault(T left, String operator, T right) {
        return ofDefault(left, Operator.of(operator), right);
    }
    static <T> Expression<T> ofDefault(T left, Operator operator, T right) {
        return new DefaultExpression<>(left, operator, right);
    }
    default Expression<String> requireRightAsExpression() {
        final String rhs = requireRight().toString();
        return Expression.ofDefault(StringExprUtil.without(rhs, "{", "}"));
    }
    default <U> Expression<U> with(U left, U right) {
        return Expression.ofDefault(left, getOperator(), right);
    }
    default Expression<OPERAND_TYPE> flipOperator() {
        return Expression.ofDefault(
                getLeftOrDefault(null),
                getOperator().flip(),
                getRightOrDefault(null));
    }
    default OPERAND_TYPE requireLeft() { return Objects.requireNonNull(getLeftOrDefault(null)); }
    default OPERAND_TYPE requireRight() { return Objects.requireNonNull(getRightOrDefault(null)); }
    String getId();
    OPERAND_TYPE getLeftOrDefault(OPERAND_TYPE resultIfNone);
    Operator getOperator();
    OPERAND_TYPE getRightOrDefault(OPERAND_TYPE resultIfNone);
}
