package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.matcher.Matcher;
import io.github.poshjosh.ratelimiter.matcher.Matchers;

public interface ExpressionMatchers {

    ExpressionMatcher<Object> MATCH_NONE = new ExpressionMatcher<Object>() {
        @Override public String match(Object toMatch) { return Matchers.NO_MATCH; }
        @Override public ExpressionMatcher<Object> matcher(Expression<String> expression) {
            return this;
        }
        @Override public boolean isSupported(Expression<String> expression) {
            return true;
        }
        @Override public String toString() { return "ExpressionMatcher$MATCH_NONE"; }
    };

    @SuppressWarnings("unchecked")
    static <T> ExpressionMatcher<T> matchNone() {
        return (ExpressionMatcher<T>) ExpressionMatchers.MATCH_NONE;
    }

    static <R> Matcher<R> matcher(String expression) {
        return ExpressionMatchers.<R>ofDefaults().matcher(expression)
                .orElseThrow(() ->
                        new IllegalArgumentException("Not a valid expression: " + expression +
                                ". For valid expressions see: https://github.com/poshjosh/rate-limiter/blob/master/docs/RATE-CONDITION-EXPRESSION-LANGUAGE.md"));
    }

    ExpressionMatcher<Object> DEFAULT = any(
            ofJvmMemory(), ofSystemTime(), ofSystemTimeElapsed(),
            ofJvmThread(), ofSystemProperty(), ofSystemEnvironment(),
            ofContainer());

    @SuppressWarnings("unchecked")
    static <R> ExpressionMatcher<R> ofDefaults() {
        return (ExpressionMatcher<R>)DEFAULT;
    }

    @SafeVarargs
    static <R> ExpressionMatcher<R> any(ExpressionMatcher<R>... matchers) {
        if (matchers.length == 1) {
            return matchers[0];
        }
        return new ExpressionMatcherComposite<>(matchers);
    }

    static <R> ExpressionMatcher<R> ofJvmMemory() {
        return ofDefaults(ExpressionParsers.ofJvmMemory(),
                ExpressionResolvers.ofLong(),
                JvmMemoryExpressionParser.MEMORY_MAX+" = ");
    }

    static <R> ExpressionMatcher<R> ofSystemTime() {
        return ofDefaults(ExpressionParsers.ofSystemTime(),
                ExpressionResolvers.ofDateTime(),
                SystemTimeExpressionParser.TIME+" = ");
    }

    static <R> ExpressionMatcher<R> ofSystemTimeElapsed() {
        return ofDefaults(ExpressionParsers.ofSystemTimeElapsed(),
                ExpressionResolvers.ofLong(),
                SystemTimeElapsedExpressionParser.TIME_ELAPSED+" = ");
    }

    static <R> ExpressionMatcher<R> ofSystemProperty() {
        return ofDefaults(ExpressionParsers.ofSystemProperty(),
                ExpressionResolvers.ofString(),
                SystemPropertyExpressionParser.LHS+" = ");
    }

    static <R> ExpressionMatcher<R> ofSystemEnvironment() {
        return ofDefaults(ExpressionParsers.ofSystemEnvironment(),
                ExpressionResolvers.ofString(),
                SystemEnvironmentExpressionParser.LHS+" = ");
    }

    static <R> ExpressionMatcher<R> ofJvmThread() {
        return ofDefaults(ExpressionParsers.ofJvmThread(),
                ExpressionResolvers.ofJvmThread(),
                JvmThreadExpressionParser.COUNT+" = ");
    }

    static ExpressionMatcher<Object> ofContainer() {
        return ofDefaults(ExpressionParsers.ofContainer(),
                ExpressionResolvers.ofContainer(),
                JvmThreadExpressionParser.COUNT+" in ");
    }

    static <R, T> ExpressionMatcher<R> ofDefaults(ExpressionParser<R, T> expressionParser,
            ExpressionResolver<T> expressionResolver, String sampleExpression) {
        return ofParseAhead(expressionParser, expressionResolver, Expressions.of(sampleExpression));
    }

    static <R, T> ExpressionMatcher<R> ofParseAhead(ExpressionParser<R, T> expressionParser,
            ExpressionResolver<T> expressionResolver, Expression<String> sampleExpression) {
        return new ParseAheadExpressionMatcher<>(expressionParser, expressionResolver, sampleExpression);
    }

    static <R, T> ExpressionMatcher<R> ofParseAtMatchTime(ExpressionParser<R, T> expressionParser,
            ExpressionResolver<T> expressionResolver, Expression<String> sampleExpression) {
        return new ParseAtMatchTimeExpressionMatcher<>(expressionParser, expressionResolver, sampleExpression);
    }
}
