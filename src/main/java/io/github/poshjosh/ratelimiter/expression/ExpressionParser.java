package io.github.poshjosh.ratelimiter.expression;

/**
 * Parses expression of one type to another type
 * @param <CONTEXT> The type of the context e.g a web request (web) or a system (sys) etc
 * @param <OPERAND> The type of the operands in the resulting expression
 */
public interface ExpressionParser<CONTEXT, OPERAND> {

    /**
     * @param expression the expression to check if supported
     * @return true if the provided expression is supported
     * @see #isSupported(Expression java.lang.String)
     */
    default boolean isSupported(String expression) {
        return isSupported(Expressions.of(expression));
    }

    /**
     * @param expression the expression to check if supported
     * @return true if the provided expression is supported
     */
    boolean isSupported(Expression<String> expression);

    /**
     * Parse a string expression into another type of expression
     * @return the result of parsing a string expression into another type
     */
    default Expression<OPERAND> parse(CONTEXT context, Expression<String> expression) {
        return Expressions.of(
                parseLeft(context, expression), expression.getOperator(), parseRight(expression));
    }

    OPERAND parseLeft(CONTEXT context, Expression<String> expression);

    OPERAND parseRight(Expression<String> expression);
}
