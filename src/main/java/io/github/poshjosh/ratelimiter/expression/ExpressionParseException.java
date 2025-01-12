package io.github.poshjosh.ratelimiter.expression;

public class ExpressionParseException extends ExpressionException {

    public ExpressionParseException() { }

    public ExpressionParseException(String message) {
        super(message);
    }

    public ExpressionParseException(String message, Throwable cause) {
        super(message, cause);
    }
}
