package io.github.poshjosh.ratelimiter;

final class Checks {
    private Checks() { }
    static void requirePositive(double amount, String what) {
        requireTrue(amount > 0, "Must be positive, %s: %d", what, amount);
    }

    static void requireTrue(boolean expression, String errorMessageFormat, Object... args) {
        if (!expression) {
            throw new IllegalArgumentException(String.format(errorMessageFormat, args));
        }
    }
}
