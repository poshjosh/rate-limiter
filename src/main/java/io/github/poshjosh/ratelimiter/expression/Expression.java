package io.github.poshjosh.ratelimiter.expression;

import java.util.Objects;

public interface Expression<T> {
    Expression<Object> TRUE = of(null, Operator.EQUALS, null);
    Expression<Object> FALSE = TRUE.flipOperator();

    static Expression<String> of(String expression) {
        return StringExprUtil.splitIntoExpression(expression);
    }
    static <T> Expression<T> of(T left, String operator, T right) {
        return of(left, Operator.of(operator), right);
    }
    static <T> Expression<T> of(T left, Operator operator, T right) {
        return new DefaultExpression<>(left, operator, right);
    }
    default Expression<String> requireRightAsExpression() {
        final String rhs = requireRight().toString();
        return Expression.of(StringExprUtil.without(rhs, "{", "}"));
    }
    default <U> Expression<U> with(U left, U right) {
        return Expression.of(left, getOperator(), right);
    }
    default Expression<T> flipOperator() {
        return Expression.of(
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
