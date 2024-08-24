package io.github.poshjosh.ratelimiter.matcher;

import java.util.Objects;

final class AndMatcher<INPUT> implements Matcher<INPUT> {

    private final Matcher<? super INPUT> left;
    private final Matcher<? super INPUT> right;

    AndMatcher(Matcher<? super INPUT> left, Matcher<? super INPUT> right) {
        this.left = Objects.requireNonNull(left);
        this.right = Objects.requireNonNull(right);
    }

    @Override
    public String match(INPUT input) {
        final String result = left.match(input);
        // If there was no match, do not continue
        if(!Matcher.isMatch(result)) {
            return Matchers.NO_MATCH;
        }
        final String afterResult = right.match(input);
        if(!Matcher.isMatch(afterResult)) {
            return Matchers.NO_MATCH;
        }
        return Matcher.composeResults(result, afterResult);
    }

    @Override
    public String toString() {
        return "AndMatcher{" + "l=" + left + ", r=" + right + '}';
    }
}
