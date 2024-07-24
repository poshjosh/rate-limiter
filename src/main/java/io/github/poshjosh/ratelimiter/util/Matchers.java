package io.github.poshjosh.ratelimiter.util;

import io.github.poshjosh.ratelimiter.expression.ExpressionMatchers;

public interface Matchers {
    String NO_MATCH = "";
    Matcher<Object> MATCH_NONE = new Matcher<Object>() {
        @Override public String match(Object input) { return NO_MATCH; }
        @Override public String toString() { return Matcher.class.getSimpleName() + "$MATCH_NONE"; }
    };

    @SuppressWarnings("unchecked")
    static <T> Matcher<T> matchNone() {
        return (Matcher<T>)MATCH_NONE;
    }

    static <T> Matcher<T> ofExpression(String expression) {
        return ExpressionMatchers.matcher(expression);
    }
}
