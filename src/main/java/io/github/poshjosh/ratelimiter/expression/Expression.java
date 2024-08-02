package io.github.poshjosh.ratelimiter.expression;

import java.util.Objects;

public interface Expression<OPERAND_TYPE> {

    default Expression<String> requireRightAsExpression() {
        final String rhs = requireRight().toString();
        return Expressions.of(StringExprUtil.without(rhs, "{", "}"));
    }
    default <U> Expression<U> with(U left, U right) {
        return Expressions.of(left, getOperator(), right);
    }
    default Expression<OPERAND_TYPE> flipOperator() {
        return Expressions.of(
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
