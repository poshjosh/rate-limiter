package io.github.poshjosh.ratelimiter.expression;

import java.util.Objects;

class DefaultExpression<T> implements Expression<T> {

    private final T left;
    private final Operator operator;
    private final T right;
    private final String id;

    DefaultExpression(T left, Operator operator, T right) {
        this.left = left;
        this.operator = Objects.requireNonNull(operator);
        this.right = right; // Nullable
        this.id = StringExprUtil.buildId(left, operator, right);
    }

    public T getLeftOrDefault(T resultIfNone) { return left == null ? resultIfNone: left; }

    public Operator getOperator() {
        return operator;
    }

    public T getRightOrDefault(T resultIfNone) { return right == null ? resultIfNone: right; }

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
