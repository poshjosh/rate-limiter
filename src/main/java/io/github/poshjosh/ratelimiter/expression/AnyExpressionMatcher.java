package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.Matcher;
import io.github.poshjosh.ratelimiter.util.Matchers;

import java.util.Arrays;

final class AnyExpressionMatcher<TARGET> implements ExpressionMatcher<TARGET>{

    private final ExpressionMatcher<TARGET>[] expressionMatchers;

    AnyExpressionMatcher(ExpressionMatcher<TARGET>[] matchers) {
        this.expressionMatchers = new ExpressionMatcher[matchers.length];
        System.arraycopy(matchers, 0, this.expressionMatchers, 0, matchers.length);
    }

    @Override
    public String match(TARGET target) {
        for (ExpressionMatcher<TARGET> expressionMatcher : expressionMatchers) {
            final String match = expressionMatcher.match(target);
            if (Matcher.isMatch(match)) {
                return match;
            }
        }
        return Matchers.NO_MATCH;
    }

    @Override
    public ExpressionMatcher<TARGET> matcher(Expression<String> expression) {
        for (ExpressionMatcher<TARGET> expressionMatcher : expressionMatchers) {
            if (expressionMatcher.isSupported(expression)) {
                return expressionMatcher.matcher(expression);
            }
        }
        throw Checks.notSupported(this, expression);
    }

    @Override
    public boolean isSupported(Expression<String> expression) {
        for (ExpressionMatcher<TARGET> expressionMatcher : expressionMatchers) {
            if (expressionMatcher.isSupported(expression)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public String toString() {
        return "AnyExpressionMatcher{matchers=" + Arrays.toString(expressionMatchers) + '}';
    }
}
