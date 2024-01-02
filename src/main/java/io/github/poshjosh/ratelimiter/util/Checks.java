package io.github.poshjosh.ratelimiter.util;

final class Checks {
    private Checks() { }
    static RuntimeException notSupported(Class<?> complainer, Object unsupported) {
        return new UnsupportedOperationException(
                complainer.getSimpleName() + " does not support: " + unsupported
        );
    }

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
