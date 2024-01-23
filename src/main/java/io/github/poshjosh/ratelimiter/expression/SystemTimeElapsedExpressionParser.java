package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.StringUtils;

import java.time.Duration;

final class SystemTimeElapsedExpressionParser<S> implements ExpressionParser<S, Long> {

    private static long TIME_AT_STARTUP = System.currentTimeMillis();
    public static final String TIME_ELAPSED = "sys.time.elapsed";

    SystemTimeElapsedExpressionParser() {}

    @Override
    public boolean isSupported(Expression<String> expression) {
        final String lhs = expression.requireLeft();
        if (TIME_ELAPSED.equals(lhs)) {
            return expression.getOperator().isType(Operator.Type.COMPARISON);
        }
        return false;
    }

    @Override
    public Long parseLeft(S source, Expression<String> expression) {
        final String lhs = expression.requireLeft();
        if (TIME_ELAPSED.equals(lhs)) {
            return System.currentTimeMillis() - getStartTime(source);
        }
        throw Checks.notSupported(this, lhs);
    }

    private long getStartTime(S source) {
        if (source instanceof Long) {
            return (Long)source;
        }
        // Memory intensive, as revealed by profiler
        // We never needed to do this in the first place.
//        if (source instanceof String) {
//            final String sval = (String)source;
//            if (StringUtils.hasText(sval)) {
//                try {
//                    return Long.parseLong(sval);
//                } catch (NumberFormatException e) {
//                    return TIME_AT_STARTUP;
//                }
//            }
//        }
        return TIME_AT_STARTUP;
    }

    @Override
    public Long parseRight(Expression<String> expression) {
        final String lhs = expression.requireLeft();
        if (TIME_ELAPSED.equals(lhs)) {
            final String rhs = expression.getRightOrDefault("");
            return StringUtils.hasText(rhs) ? Duration.parse(rhs).toMillis() : null;
        }
        throw Checks.notSupported(this, lhs);
    }
}
