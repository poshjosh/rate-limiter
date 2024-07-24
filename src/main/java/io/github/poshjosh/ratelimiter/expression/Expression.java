package io.github.poshjosh.ratelimiter.expression;

import java.util.Objects;

public interface Expression<T> {
    Expression<Object> TRUE = ofDefault(null, Operator.EQUALS, null);
    Expression<Object> FALSE = TRUE.flipOperator();

    static Expression<String> ofDefault(String expression) {
        return StringExprUtil.splitIntoExpression(expression);
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
    default Expression<T> flipOperator() {
        return Expression.ofDefault(
                getLeftOrDefault(null),
                getOperator().flip(),
                getRightOrDefault(null));
    }
    default T requireLeft() { return Objects.requireNonNull(getLeftOrDefault(null)); }
    default T requireRight() { return Objects.requireNonNull(getRightOrDefault(null)); }
    String getId();
    T getLeftOrDefault(T resultIfNone);
    Operator getOperator();
    T getRightOrDefault(T resultIfNone);
}
