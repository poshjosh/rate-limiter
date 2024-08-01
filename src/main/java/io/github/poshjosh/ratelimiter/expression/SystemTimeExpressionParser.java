package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.StringUtils;

import java.time.LocalDateTime;

final class SystemTimeExpressionParser<CONTEXT> implements ExpressionParser<CONTEXT, LocalDateTime> {

    public static final String TIME = "sys.time";

    SystemTimeExpressionParser() {}

    @Override
    public boolean isSupported(Expression<String> expression) {
        final String lhs = expression.requireLeft();
        if (TIME.equals(lhs)) {
            return expression.getOperator().isType(Operator.Type.COMPARISON);
        }
        return false;
    }

    @Override
    public LocalDateTime parseLeft(CONTEXT context, Expression<String> expression) {
        final String lhs = expression.requireLeft();
        if (TIME.equals(lhs)) {
            return LocalDateTime.now();
        }
        throw Checks.notSupported(this, lhs);
    }

    @Override
    public LocalDateTime parseRight(Expression<String> expression) {
        final String lhs = expression.requireLeft();
        if (TIME.equals(lhs)) {
            final String rhsText = expression.getRightOrDefault("");
            if (StringUtils.hasText(rhsText)) {
                return LocalDateTime.parse(rhsText);
            }
            return null;
        }
        throw Checks.notSupported(this, lhs);
    }
}
