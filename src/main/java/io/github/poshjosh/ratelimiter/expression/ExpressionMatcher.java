package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.Operator;
import io.github.poshjosh.ratelimiter.util.Matcher;
import io.github.poshjosh.ratelimiter.util.StringUtils;

import java.time.LocalDateTime;
import java.util.Optional;

public interface ExpressionMatcher<INPUT, T> extends Matcher<INPUT> {

    ExpressionMatcher<Object, Object> MATCH_NONE = new ExpressionMatcher<Object, Object>() {
        @Override public String match(Object request) { return Matcher.NO_MATCH; }
        @Override public ExpressionMatcher<Object, Object> matcher(Expression<String> expression) {
            return this;
        }
        @Override public boolean isSupported(Expression<String> expression) {
            return true;
        }
        @Override public String toString() { return "ExpressionMatcher$MATCH_NONE"; }
    };

    @SuppressWarnings("unchecked")
    static <T, K> ExpressionMatcher<T, K> matchNone() {
        return (ExpressionMatcher<T, K>)MATCH_NONE;
    }

    static <R> ExpressionMatcher<R, Object> ofDefault() {
        return any(ofSystemMemory(), ofSystemTime(), ofSystemTimeElapsed(),
                ofJvmThread(), ofSystemProperty(), ofSystemEnvironment());
    }

    static <R> ExpressionMatcher<R, Object> any(ExpressionMatcher<R, ?>... matchers) {
        return new AnyExpressionMatcher<>(matchers);
    }

    static <R> ExpressionMatcher<R, Long> ofSystemMemory() {
        return ofDefaults(ExpressionParser.ofJvmMemory(),
                ExpressionResolver.ofLong(),
                JvmMemoryExpressionParser.MEMORY_MAX+"=");
    }

    static <R> ExpressionMatcher<R, LocalDateTime> ofSystemTime() {
        return ofDefaults(ExpressionParser.ofSystemTime(),
                ExpressionResolver.ofDateTime(),
                SystemTimeExpressionParser.TIME+"=");
    }

    static <R> ExpressionMatcher<R, Long> ofSystemTimeElapsed() {
        return ofDefaults(ExpressionParser.ofSystemTimeElapsed(),
                ExpressionResolver.ofLong(),
                SystemTimeElapsedExpressionParser.TIME_ELAPSED+"=");
    }

    static <R> ExpressionMatcher<R, String> ofSystemProperty() {
        return ofDefaults(ExpressionParser.ofSystemProperty(),
                ExpressionResolver.ofString(),
                SystemPropertyExpressionParser.LHS+"=");
    }


    static <R> ExpressionMatcher<R, String> ofSystemEnvironment() {
        return ofDefaults(ExpressionParser.ofSystemEnvironment(),
                ExpressionResolver.ofString(),
                SystemEnvironmentExpressionParser.LHS+"=");
    }

    static <R> ExpressionMatcher<R, Object> ofJvmThread() {
        return ofDefaults(ExpressionParser.ofJvmThread(),
                ExpressionResolver.ofJvmThread(),
                JvmThreadExpressionParser.COUNT+"=");
    }

    static <R, T> ExpressionMatcher<R, T> ofDefaults(
            ExpressionParser<R, T> expressionParser,
            ExpressionResolver<T> expressionResolver,
            String sampleExpression) {
        return ofParseAhead(expressionParser, expressionResolver, Expression.of(sampleExpression));
    }

    static <R, T> ExpressionMatcher<R, T> ofParseAhead(
            ExpressionParser<R, T> expressionParser,
            ExpressionResolver<T> expressionResolver,
            Expression<String> sampleExpression) {
        return new ParseAheadExpressionMatcher<>(expressionParser, expressionResolver, sampleExpression);
    }

    static <R, T> ExpressionMatcher<R, T> ofParseAtMatchTime(
            ExpressionParser<R, T> expressionParser,
            ExpressionResolver<T> expressionResolver,
            Expression<String> sampleExpression) {
        return new ParseAtMatchTimeExpressionMatcher<>(expressionParser, expressionResolver, sampleExpression);
    }

    @Override String match(INPUT request);

    ExpressionMatcher<INPUT, T> matcher(Expression<String> expression);

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
