package io.github.poshjosh.ratelimiter.expression;

import java.time.LocalDateTime;
import java.util.Objects;

class DateTimeExpressionResolver extends AbstractExpressionResolver<LocalDateTime>{
    DateTimeExpressionResolver() {}

    protected boolean resolvePositive(LocalDateTime left, Operator operator, LocalDateTime right) {
        if (!"".equals(operator.getSymbol())) {
            Objects.requireNonNull(left);
            Objects.requireNonNull(right);
        }
        switch (operator.getSymbol()) {
            case "=":
                if (left == null && right == null) {
                    return true;
                }
                if (left == null || right == null) {
                    return false;
                }
                return left.isEqual(right);
            case ">":
                return left.isAfter(right);
            case ">=":
                return left.isAfter(right) || left.isEqual(right);
            case "<":
                return left.isBefore(right);
            case "<=":
                return left.isBefore(right) || left.isEqual(right);
            default:
                throw Checks.notSupported(this, operator);
        }
    }

    @Override
    public boolean isSupported(Operator operator) {
        return operator.isType(Operator.Type.COMPARISON);
    }
}
