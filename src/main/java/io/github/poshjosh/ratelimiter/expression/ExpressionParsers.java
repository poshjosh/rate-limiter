package io.github.poshjosh.ratelimiter.expression;

import java.time.LocalDateTime;

public final class ExpressionParsers {
    private ExpressionParsers() { }
    @SafeVarargs
    @SuppressWarnings("unchecked")
    public static <C> ExpressionParser<C, Object> any(ExpressionParser<C, ?>... parsers) {
        if (parsers.length == 1) {
            return (ExpressionParser<C, Object>)parsers[0];
        }
        return new AnyExpressionParser(parsers);
    }

    static final ExpressionParser<?, Long> JVM_MEMORY = new JvmMemoryExpressionParser<>();
    @SuppressWarnings("unchecked")
    public static <C> ExpressionParser<C, Long> ofJvmMemory() {
        return (ExpressionParser<C, Long>)JVM_MEMORY;
    }

    static final ExpressionParser<?, LocalDateTime> TIME = new SystemTimeExpressionParser<>();
    @SuppressWarnings("unchecked")
    public static <C> ExpressionParser<C, LocalDateTime> ofSystemTime() {
        return (ExpressionParser<C, LocalDateTime>)TIME;
    }

    static final ExpressionParser<?, Long> TIME_ELAPSED = new SystemTimeElapsedExpressionParser<>();
    @SuppressWarnings("unchecked")
    public static <C> ExpressionParser<C, Long> ofSystemTimeElapsed() {
        return (ExpressionParser<C, Long>)TIME_ELAPSED;
    }

    static final ExpressionParser<?, String> SYS_PROP = new SystemPropertyExpressionParser<>();
    @SuppressWarnings("unchecked")
    public static <C> ExpressionParser<C, String> ofSystemProperty() {
        return (ExpressionParser<C, String>)SYS_PROP;
    }

    static final ExpressionParser<?, String> SYS_ENV = new SystemEnvironmentExpressionParser<>();
    @SuppressWarnings("unchecked")
    public static <C> ExpressionParser<C, String> ofSystemEnvironment() {
        return (ExpressionParser<C, String>)SYS_ENV;
    }

    static final ExpressionParser<?, Object> JVM_THREAD = new JvmThreadExpressionParser<>();
    @SuppressWarnings("unchecked")
    public static <C> ExpressionParser<C, Object> ofJvmThread() {
        return (ExpressionParser<C, Object>)JVM_THREAD;
    }

    static final ExpressionParser<?, Object> CONTAINER = new ContainerExpressionParser<>();
    @SuppressWarnings("unchecked")
    public static <C> ExpressionParser<C, Object> ofContainer() {
        return (ExpressionParser<C, Object>)CONTAINER;
    }
}
