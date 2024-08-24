package io.github.poshjosh.ratelimiter.matcher;

@FunctionalInterface
public interface Matcher<INPUT> {
    static String composeResults(String first, String second) {
        final boolean firstIsMatch = isMatch(first);
        final boolean secondIsMatch = isMatch(second);
        if (!firstIsMatch && !secondIsMatch) {
            return Matchers.NO_MATCH;
        }
        if (!firstIsMatch) {
            return second;
        }
        if (!secondIsMatch) {
            return first;
        }
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
        return new AndMatcher<>(this, after);
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
        return new OrMatcher<>(this, after);
    }
}
