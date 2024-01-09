package io.github.poshjosh.ratelimiter.util;

import java.util.Objects;

final class LiteralMatcher<INPUT> implements Matcher<INPUT> {
    private final String text;

    LiteralMatcher(String text) {
        this.text = text;
    }

    @Override
    public String match(INPUT input) {
        return Objects.equals(text, input) ? text : Matcher.NO_MATCH;
    }

    @Override
    public String toString() {
        return "LiteralMatcher{" + "text='" + text + '\'' + '}';
    }
}
