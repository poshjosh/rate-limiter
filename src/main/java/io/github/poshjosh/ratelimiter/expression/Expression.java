package io.github.poshjosh.ratelimiter.expression;

import java.util.Objects;

public interface Expression<OPERAND> {
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
