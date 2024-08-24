package io.github.poshjosh.ratelimiter.matcher;

import java.util.Objects;

final class OrMatcher<INPUT> implements Matcher<INPUT> {

    private final Matcher<? super INPUT> left;
    private final Matcher<? super INPUT> right;

    OrMatcher(Matcher<? super INPUT> left, Matcher<? super INPUT> right) {
        this.left = Objects.requireNonNull(left);
        this.right = Objects.requireNonNull(right);
    }

    @Override
    public String match(INPUT input) {
        final String leftMatch = left.match(input);
        final boolean leftIsMatch = Matcher.isMatch(leftMatch);
        final String rightMatch = right.match(input);
        final boolean rightIsMatch = Matcher.isMatch(rightMatch);
        if (leftIsMatch) {
            if (rightIsMatch) {
                return Matcher.composeResults(leftMatch, rightMatch);
            } else {
                return leftMatch;
            }
        } else {
            if (rightIsMatch) {
                return rightMatch;
            } else {
                return Matchers.NO_MATCH;
            }
        }
    }

    @Override
    public String toString() {
        return "OrMatcher{" + "l=" + left + ", r=" + right + '}';
    }
}
