package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.matcher.Matcher;
import io.github.poshjosh.ratelimiter.matcher.Matchers;

import java.util.Arrays;
import java.util.Objects;

final class ExpressionMatcherComposite<INPUT> implements ExpressionMatcher<INPUT>{

    private final ExpressionMatcher<INPUT>[] expressionMatchers;

    ExpressionMatcherComposite(ExpressionMatcher<INPUT>[] expressionMatchers) {
        this.expressionMatchers = Objects.requireNonNull(expressionMatchers);
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
        return "ExpressionMatcherComposite{matchers=" + Arrays.toString(expressionMatchers) + '}';
    }
}
