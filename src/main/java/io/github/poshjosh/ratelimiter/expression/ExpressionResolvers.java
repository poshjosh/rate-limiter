package io.github.poshjosh.ratelimiter.expression;

import java.time.LocalDateTime;

public final class ExpressionResolvers {
    private ExpressionResolvers() { }

    static final ExpressionResolver<Long> LONG = new LongExpressionResolver();
    public static ExpressionResolver<Long> ofLong() { return LONG; }

    static final ExpressionResolver<Double> DECIMAL = new DecimalExpressionResolver();
    public static ExpressionResolver<Double> ofDecimal() { return DECIMAL; }

    static final ExpressionResolver<String> STRING = new StringExpressionResolver();
    public static ExpressionResolver<String> ofString() { return STRING; }

    static final ExpressionResolver<LocalDateTime> DATE_TIME = new DateTimeExpressionResolver();
    public static ExpressionResolver<LocalDateTime> ofDateTime() { return DATE_TIME; }

    static final ExpressionResolver<Object> JVM_THREAD = new JvmThreadExpressionResolver();
    public static ExpressionResolver<Object> ofJvmThread() { return JVM_THREAD; }

    static final ExpressionResolver<Object> CONTAINER = new ContainerExpressionResolver();
    public static ExpressionResolver<Object> ofContainer() { return CONTAINER; }
}
