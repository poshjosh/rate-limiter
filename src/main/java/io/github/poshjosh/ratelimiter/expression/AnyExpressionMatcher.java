package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.Matcher;
import io.github.poshjosh.ratelimiter.util.Matchers;

import java.util.Arrays;

final class AnyExpressionMatcher<INPUT> implements ExpressionMatcher<INPUT>{

    private final ExpressionMatcher<INPUT>[] expressionMatchers;

    AnyExpressionMatcher(ExpressionMatcher<INPUT>[] matchers) {
        this.expressionMatchers = new ExpressionMatcher[matchers.length];
        System.arraycopy(matchers, 0, this.expressionMatchers, 0, matchers.length);
    }

    @Override
    public String match(INPUT toMatch) {
        for (ExpressionMatcher<INPUT> expressionMatcher : expressionMatchers) {
            final String match = expressionMatcher.match(toMatch);
            if (Matcher.isMatch(match)) {
                return match;
            }
        }
        return Matchers.NO_MATCH;
    }

    @Override
    public ExpressionMatcher<INPUT> matcher(Expression<String> expression) {
        for (ExpressionMatcher<INPUT> expressionMatcher : expressionMatchers) {
            if (expressionMatcher.isSupported(expression)) {
                return expressionMatcher.matcher(expression);
            }
        }
        throw Checks.notSupported(this, expression);
    }

    @Override
    public boolean isSupported(Expression<String> expression) {
        for (ExpressionMatcher<INPUT> expressionMatcher : expressionMatchers) {
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
