package io.github.poshjosh.ratelimiter.util;

import io.github.poshjosh.ratelimiter.expression.ExpressionMatcher;

import java.util.Objects;

@FunctionalInterface
public interface Matcher<INPUT> {

    String NO_MATCH = "";

    Matcher<Object> MATCH_NONE = new Matcher<Object>() {
        @Override public String match(Object target) { return Matcher.NO_MATCH; }
        @Override public String toString() { return Matcher.class.getSimpleName() + "$MATCH_NONE"; }
    };

    @SuppressWarnings("unchecked")
    static <T> Matcher<T> matchNone() {
        return (Matcher<T>)MATCH_NONE;
    }

    static <T> Matcher<T> ofExpression(String expression) {
        return ExpressionMatcher.<T>ofDefault().matcher(expression)
                .orElseThrow(() ->
                        new IllegalArgumentException("Not a valid expression: " + expression +
                                ". For valid expressions see: https://github.com/poshjosh/rate-limiter/blob/master/docs/RATE-CONDITION-EXPRESSION-LANGUAGE.md"));
    }

    static String composeResults(String first, String second) {
        return first + '_' + second;
    }
    static boolean isMatch(String matchResult) { return !matchResult.isEmpty(); }

    default boolean matches(INPUT input) { return isMatch(match(input)); }

    /**
     * Match the input. Return a match, or empty text, if there is no match.
     * @param input The input to match
     * @return A matching string, or empty text, if none.
     */
    String match(INPUT input);

    /**
     * Returns a composed {@code Matcher} that returns a composed match result
     * only if both matchers succeed.
     *
     * <p>Compose a {@code Matcher} that performs, in sequence, this operation followed by the
     * {@code after} operation. If performing either operation throws an exception, it is relayed
     * to the caller of the composed operation. If performing this operation throws an exception,
     * the {@code after} operation will not be performed. Likewise, if this operation does not
     * match, the {@code after} operation will not be performed.
     *
     * @param after the after operation to perform
     * @return a composed {@code Matcher}
     * @throws NullPointerException if {@code after} is null
     */
    default Matcher<INPUT> and(Matcher<? super INPUT> after) {
        Objects.requireNonNull(after);
        return (INPUT t) -> {
            final String result = match(t);
            // If there was no match, do not continue
            if(!isMatch(result)) {
                return NO_MATCH;
            }
            final String afterResult = after.match(t);
            if(!isMatch(afterResult)) {
                return NO_MATCH;
            }
            return composeResults(result, afterResult);
        };
    }

    /**
     * Returns a composed {@code Matcher} that returns a match result composed of the results
     * of successful matchers.
     *
     * <p>Compose a {@code Matcher} that performs, in sequence, this operation followed by the
     * {@code after} operation. If performing either operation throws an exception, it is relayed
     * to the caller of the composed operation. If performing this operation throws an exception,
     * the {@code after} operation will not be performed. If no exceptions are throw, then both
     * operations will be called. However, only the result of successful operations will be returned.
     *
     * @param after the after operation to perform
     * @return a composed {@code Matcher}
     * @throws NullPointerException if {@code after} is null
     */
    default Matcher<INPUT> or(Matcher<? super INPUT> after) {
        Objects.requireNonNull(after);
        return (INPUT t) -> {
            final String result = match(t);
            final boolean resultMatch = isMatch(result);
            final String afterResult = after.match(t);
            final boolean afterResultMatch = isMatch(afterResult);
            if (resultMatch) {
                if (afterResultMatch) {
                    return composeResults(result, afterResult);
                } else {
                    return result;
                }
            } else {
                if (afterResultMatch) {
                    return afterResult;
                } else {
                    return NO_MATCH;
                }
            }
        };
    }
}
