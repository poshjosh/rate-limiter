package io.github.poshjosh.ratelimiter.expression;

import java.util.Objects;

class DefaultExpression<OPERAND_TYPE> implements Expression<OPERAND_TYPE> {

    private final OPERAND_TYPE left;
    private final Operator operator;
    private final OPERAND_TYPE right;
    private final String id;

    DefaultExpression(OPERAND_TYPE left, Operator operator, OPERAND_TYPE right) {
        this.left = left;
        this.operator = Objects.requireNonNull(operator);
        this.right = right; // Nullable
        this.id = StringExprUtil.buildId(left, operator, right);
    }

    public OPERAND_TYPE getLeftOrDefault(OPERAND_TYPE resultIfNone) { return left == null ? resultIfNone: left; }

    public Operator getOperator() {
        return operator;
    }

    public OPERAND_TYPE getRightOrDefault(OPERAND_TYPE resultIfNone) { return right == null ? resultIfNone: right; }

    // TODO - If this is really an ID, why are we not using it in equals() and hashCode():
    public String getId() { return id; }

    @Override public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        DefaultExpression<?> that = (DefaultExpression<?>) o;
        return Objects.equals(left, that.left) && operator.equals(that.operator) && Objects
                .equals(right, that.right);
    }

    @Override public int hashCode() {
        return Objects.hash(left, operator, right);
    }

    @Override
    public String toString() {
        return getId();
    }
}
