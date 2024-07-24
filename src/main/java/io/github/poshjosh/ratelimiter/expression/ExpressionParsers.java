package io.github.poshjosh.ratelimiter.expression;

import java.time.LocalDateTime;

public interface ExpressionParsers {
    static <C> ExpressionParser<C, Long> ofJvmMemory() {
        return new JvmMemoryExpressionParser<>();
    }

    static <C> ExpressionParser<C, LocalDateTime> ofSystemTime() {
        return new SystemTimeExpressionParser<>();
    }

    static <C> ExpressionParser<C, Long> ofSystemTimeElapsed() {
        return new SystemTimeElapsedExpressionParser<>();
    }

    static <C> ExpressionParser<C, String> ofSystemProperty() {
        return new SystemPropertyExpressionParser<>();
    }

    static <C> ExpressionParser<C, String> ofSystemEnvironment() {
        return new SystemEnvironmentExpressionParser<>();
    }

    static <C> ExpressionParser<C, Object> ofJvmThread() {
        return new JvmThreadExpressionParser<>();
    }
}
