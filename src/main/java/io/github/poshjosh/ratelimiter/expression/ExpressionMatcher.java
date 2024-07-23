package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.Operator;
import io.github.poshjosh.ratelimiter.util.Matcher;
import io.github.poshjosh.ratelimiter.util.StringUtils;

import java.util.Optional;

public interface ExpressionMatcher<INPUT> extends Matcher<INPUT> {

    ExpressionMatcher<Object> MATCH_NONE = new ExpressionMatcher<Object>() {
        @Override public String match(Object input) { return Matcher.NO_MATCH; }
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
        return (ExpressionMatcher<T>)MATCH_NONE;
    }

    static <R> ExpressionMatcher<R> ofDefault() {
        return any(ofSystemMemory(), ofSystemTime(), ofSystemTimeElapsed(),
                ofJvmThread(), ofSystemProperty(), ofSystemEnvironment());
    }

    @SafeVarargs
    static <R> ExpressionMatcher<R> any(ExpressionMatcher<R>... matchers) {
        if (matchers.length == 1) {
            return matchers[0];
        }
        return new AnyExpressionMatcher<>(matchers);
    }

    static <R> ExpressionMatcher<R> ofSystemMemory() {
        return ofDefaults(ExpressionParser.ofJvmMemory(),
                ExpressionResolver.ofLong(),
                JvmMemoryExpressionParser.MEMORY_MAX+"=");
    }

    static <R> ExpressionMatcher<R> ofSystemTime() {
        return ofDefaults(ExpressionParser.ofSystemTime(),
                ExpressionResolver.ofDateTime(),
                SystemTimeExpressionParser.TIME+"=");
    }

    static <R> ExpressionMatcher<R> ofSystemTimeElapsed() {
        return ofDefaults(ExpressionParser.ofSystemTimeElapsed(),
                ExpressionResolver.ofLong(),
                SystemTimeElapsedExpressionParser.TIME_ELAPSED+"=");
    }

    static <R> ExpressionMatcher<R> ofSystemProperty() {
        return ofDefaults(ExpressionParser.ofSystemProperty(),
                ExpressionResolver.ofString(),
                SystemPropertyExpressionParser.LHS+"=");
    }


    static <R> ExpressionMatcher<R> ofSystemEnvironment() {
        return ofDefaults(ExpressionParser.ofSystemEnvironment(),
                ExpressionResolver.ofString(),
                SystemEnvironmentExpressionParser.LHS+"=");
    }

    static <R> ExpressionMatcher<R> ofJvmThread() {
        return ofDefaults(ExpressionParser.ofJvmThread(),
                ExpressionResolver.ofJvmThread(),
                JvmThreadExpressionParser.COUNT+"=");
    }

    static <R, T> ExpressionMatcher<R> ofDefaults(
            ExpressionParser<R, T> expressionParser,
            ExpressionResolver<T> expressionResolver,
            String sampleExpression) {
        return ofParseAhead(expressionParser, expressionResolver, Expression.of(sampleExpression));
    }

    static <R, T> ExpressionMatcher<R> ofParseAhead(
            ExpressionParser<R, T> expressionParser,
            ExpressionResolver<T> expressionResolver,
            Expression<String> sampleExpression) {
        return new ParseAheadExpressionMatcher<>(expressionParser, expressionResolver, sampleExpression);
    }

    static <R, T> ExpressionMatcher<R> ofParseAtMatchTime(
            ExpressionParser<R, T> expressionParser,
            ExpressionResolver<T> expressionResolver,
            Expression<String> sampleExpression) {
        return new ParseAtMatchTimeExpressionMatcher<>(expressionParser, expressionResolver, sampleExpression);
    }

    @Override String match(INPUT request);

    ExpressionMatcher<INPUT> matcher(Expression<String> expression);

    boolean isSupported(Expression<String> expression);

    default Optional<Matcher<INPUT>> matcher(String text) {
        if (!StringUtils.hasText(text)) {
            return Optional.empty();
        }
        String [] parts = StringExprUtil.splitIntoExpressionsAndConjunctors(text);
        if (parts.length == 0) {
            return Optional.empty();
        }
        Matcher<INPUT> result = null;
        Operator operator = Operator.NONE;
        for (int i = 0; i < parts.length; i++) {
            final String part = parts[i];
            if (i % 2 == 0) {
                final Matcher<INPUT> matcher;
                if (!StringUtils.hasText(part)) {
                    matcher = null;
                } else {
                    Expression<String> expression = Expression.of(part);
                    if (isSupported(expression)) {
                        matcher = matcher(expression);
                    } else {
                        throw Checks.notSupported(this, "expression: " + part);
                    }
                }
                if (result == null) {
                    result = matcher;
                } else {
                    switch(operator) {
                        case AND:
                            result = result.and(matcher); break;
                        case OR:
                            result = result.or(matcher); break;
                        case NONE:
                        default:
                            throw Checks.notSupported(this, "operator: " + operator);
                    }
                }
            } else {
                operator = Operator.ofSymbol(parts[i]);
            }
        }
        return Optional.ofNullable(result);
    }
}
