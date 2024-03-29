package io.github.poshjosh.ratelimiter.bandwidths;

final class Checks {
    private Checks() { }
    static RuntimeException notSupported(Object complainer, Object unsupported) {
        return notSupported(complainer.getClass(), unsupported);
    }
    static RuntimeException notSupported(Class<?> complainer, Object unsupported) {
        return new UnsupportedOperationException(
                complainer.getSimpleName() + " does not support: " + unsupported);
    }
    static void requireNotNegative(double amount, String what) {
        requireFalse(amount < 0, "Must not be negative, %s: %d", what, amount);
    }
    static void requireFalse(boolean expression, String errorMessageFormat, Object... args) {
        if (expression) {
            throw new IllegalArgumentException(String.format(errorMessageFormat, args));
        }
    }
    static void requireTrue(boolean expression, String errorMessageFormat, Object... args) {
        if (!expression) {
            throw new IllegalArgumentException(String.format(errorMessageFormat, args));
        }
    }
}
