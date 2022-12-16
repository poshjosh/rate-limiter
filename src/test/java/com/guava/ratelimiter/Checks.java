package com.guava.ratelimiter;

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

    static void requireTrue(boolean expression, String errorMessageFormat, Object... args) {
        if (!expression) {
            throw new IllegalArgumentException(String.format(errorMessageFormat, args));
        }
    }
}
