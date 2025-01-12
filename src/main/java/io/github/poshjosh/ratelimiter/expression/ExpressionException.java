package io.github.poshjosh.ratelimiter.expression;

public class ExpressionException extends RuntimeException {

    public ExpressionException() { }

    public ExpressionException(String message) {
        super(message);
    }

    public ExpressionException(String message, Throwable cause) {
        super(message, cause);
    }
}
