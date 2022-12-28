package com.looseboxes.ratelimiter;

public final class Checks {
    private Checks() { }

    public static void requirePositive(double amount, String what) {
        requireTrue(amount > 0, "Must be positive, %s: %d", what, amount);
    }

    public static void requireNotNegative(double amount, String what) {
        requireFalse(amount < 0, "Must not be negative, %s: %d", what, amount);
    }

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

    public static void requireFalse(boolean expression, String errorMessageFormat, Object... args) {
        if (expression) {
            throw new IllegalArgumentException(String.format(errorMessageFormat, args));
        }
    }

    public static void requireTrue(boolean expression, String errorMessageFormat, Object... args) {
        if (!expression) {
            throw new IllegalArgumentException(String.format(errorMessageFormat, args));
        }
    }
}
