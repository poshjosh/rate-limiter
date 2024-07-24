package io.github.poshjosh.ratelimiter.expression;

import java.time.LocalDateTime;

public interface ExpressionResolvers {
    static ExpressionResolver<Long> ofLong() { return new LongExpressionResolver(); }

    static ExpressionResolver<Double> ofDecimal() { return new DecimalExpressionResolver(); }

    static ExpressionResolver<String> ofString() { return new StringExpressionResolver(); }

    static ExpressionResolver<LocalDateTime> ofDateTime() { return new DateTimeExpressionResolver(); }

    static ExpressionResolver<Object> ofJvmThread() { return new JvmThreadExpressionResolver(); }
}
