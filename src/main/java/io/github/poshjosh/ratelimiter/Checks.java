package io.github.poshjosh.ratelimiter;

final class Checks {
    private Checks() { }
    public static RuntimeException notSupported(Class<?> complainer, Object unsupported) {
        return new UnsupportedOperationException(
                complainer.getSimpleName() + " does not support: " + unsupported
        );
    }
    public static void requirePositive(double amount, String what) {
        requireTrue(amount > 0, "Must be positive, %s: %d", what, amount);
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

    public static void requireTrue(boolean expression, String errorMessageFormat, Object... args) {
        if (!expression) {
            throw new IllegalArgumentException(String.format(errorMessageFormat, args));
        }
    }
}
