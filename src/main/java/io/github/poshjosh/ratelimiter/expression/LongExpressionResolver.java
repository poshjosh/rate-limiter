package io.github.poshjosh.ratelimiter.expression;

import java.util.Objects;

final class LongExpressionResolver extends AbstractExpressionResolver<Long> {
    LongExpressionResolver() {}

    public boolean resolvePositive(Long left, Operator operator, Long right) {
        if (!"=".equals(operator.getSymbol())) {
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
