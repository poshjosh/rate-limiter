package io.github.poshjosh.ratelimiter.util;

final class Checks {
    private Checks() { }
    static void requireFalse(boolean expression, String errorMessage) {
        if (expression) {
            throw new IllegalArgumentException(errorMessage);
        }
    }

    static void requireTrue(boolean expression, String errorMessage) {
        if (!expression) {
            throw new IllegalArgumentException(errorMessage);
        }
    }
}
