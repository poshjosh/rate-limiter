package io.github.poshjosh.ratelimiter.matcher;

import io.github.poshjosh.ratelimiter.expression.ExpressionMatchers;

public final class Matchers {
    public static final String NO_MATCH = "";
    public static final Matcher<Object> MATCH_NONE = new Matcher<Object>() {
        @Override public String match(Object input) { return NO_MATCH; }
        @Override public String toString() { return Matcher.class.getSimpleName() + "$MATCH_NONE"; }
    };

    @SuppressWarnings("unchecked")
    public static <T> Matcher<T> matchNone() {
        return (Matcher<T>)MATCH_NONE;
    }

    public static <T> Matcher<T> ofExpression(String expression) {
        return ExpressionMatchers.matcher(expression);
    }

    private Matchers() { }
}
