package io.github.poshjosh.ratelimiter.expression;

import java.util.Objects;

final class JvmThreadExpressionResolver extends AbstractExpressionResolver<Object> {
    private final ExpressionResolver<Long> longExpressionResolver;
    JvmThreadExpressionResolver() {
        this.longExpressionResolver = ExpressionResolver.ofLong();
    }
    @Override
    public boolean resolvePositive(Object left, Operator operator, Object right) {
        if (left instanceof Long) {
            Objects.requireNonNull(left);
            Objects.requireNonNull(right);
            return longExpressionResolver.resolve((Long)left, operator, (Long)right);
        } else {
            return Objects.equals(left, right);
        }
    }

    @Override
    public boolean isSupported(Operator operator) {
        return operator.isType(Operator.Type.COMPARISON);
    }
}
