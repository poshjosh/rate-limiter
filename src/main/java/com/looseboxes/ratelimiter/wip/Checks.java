package com.looseboxes.ratelimiter.wip;

public final class Checks {
    private Checks() { }

    public static void requireFalse(boolean expression, String errorMessage) {
        if (expression) {
            throw new IllegalArgumentException(errorMessage);
        }
    }

    public static void requireTrue(boolean expression, String errorMessage) {
        if (!expression) {
            throw new IllegalArgumentException(errorMessage);
        }
    }

    public static void requireTrue(boolean expression, String errorMessageFormat, Object... args) {
        if (!expression) {
            throw new IllegalArgumentException(String.format(errorMessageFormat, args));
        }
    }
}
