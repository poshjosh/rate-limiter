package io.github.poshjosh.ratelimiter.expression;

final class Checks {
    private Checks() { }
    static RuntimeException notSupported(Object complainer, Object unsupported) {
        return notSupported(complainer.getClass(), unsupported);
    }
    static RuntimeException notSupported(Class<?> complainer, Object unsupported) {
        return new ExpressionException(
                complainer.getSimpleName() + " does not support: " + unsupported);
    }
}
