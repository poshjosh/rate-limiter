package io.github.poshjosh.ratelimiter.expression;

import java.util.Objects;

public interface Expression<OPERAND> {
    default boolean isRightAnExpression() {
        try {
            requireRightAsExpression();
            return true;
        } catch (NullPointerException | UnsupportedOperationException e) {
            return false;
        }
    }
    default Expression<String> requireRightAsExpression() {
        final String rhs = requireRight().toString();
        return Expressions.of(StringExprUtil.withoutBraces(rhs));
    }
    default <U> Expression<U> with(U left, U right) {
        return Expressions.of(left, getOperator(), right);
    }
    default Expression<OPERAND> flipOperator() {
        return Expressions.of(
                getLeftOrDefault(null),
                getOperator().flip(),
                getRightOrDefault(null));
    }
    default OPERAND requireLeft() { return Objects.requireNonNull(getLeftOrDefault(null)); }
    default OPERAND requireRight() { return Objects.requireNonNull(getRightOrDefault(null)); }
    String getId();
    OPERAND getLeftOrDefault(OPERAND resultIfNone);
    Operator getOperator();
    OPERAND getRightOrDefault(OPERAND resultIfNone);
}
