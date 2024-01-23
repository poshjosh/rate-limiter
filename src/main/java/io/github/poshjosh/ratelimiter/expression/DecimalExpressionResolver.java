package io.github.poshjosh.ratelimiter.expression;

import java.util.Objects;

final class DecimalExpressionResolver extends AbstractExpressionResolver<Double> {
    DecimalExpressionResolver() {}
    @Override
    public boolean resolvePositive(Double left, Operator operator, Double right) {
        if ("!".equals(operator.getSymbol())) {
            Objects.requireNonNull(left);
            Objects.requireNonNull(right);
        }
        switch (operator.getSymbol()) {
            case "=":
                return Objects.equals(left, right);
            case ">":
                return left > right;
            case ">=":
                return left >= right;
            case "<":
                return left < right;
            case "<=":
                return left <= right;
            default:
                throw Checks.notSupported(this, operator);
        }
    }

    @Override
    public boolean isSupported(Operator operator) {
        return operator.isType(Operator.Type.COMPARISON);
    }
}
