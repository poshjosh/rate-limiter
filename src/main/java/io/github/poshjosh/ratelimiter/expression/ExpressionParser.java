package io.github.poshjosh.ratelimiter.expression;

/**
 * Parses expression of one type to another type
 * @param <CONTEXT> The type of the context e.g a web request (web) or a system (sys) etc
 * @param <OPERAND_TYPE> The type of the resulting expression
 */
public interface ExpressionParser<CONTEXT, OPERAND_TYPE> {

    /**
     * @param expression the expression to check if supported
     * @return true if the provided expression is supported
     * @see #isSupported(Expression java.lang.String)
     */
    default boolean isSupported(String expression) {
        return isSupported(Expression.ofDefault(expression));
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
    default Expression<OPERAND_TYPE> parse(CONTEXT context, Expression<String> expression) {
        return Expression.ofDefault(
                parseLeft(context, expression), parseOperator(expression), parseRight(expression));
    }

    OPERAND_TYPE parseLeft(CONTEXT context, Expression<String> expression);

    /**
     * Parse the operator from the expression
     * For input: "sys.environment={service.instances>1}", output operator is ">" not "="
     * @param expression The expression whose operator will be returned
     * @return The operator of the expression
     */
    default Operator parseOperator(Expression<String> expression) {
        final String rhsText = expression.getRightOrDefault("");
        return Operator.getOperatorIn(rhsText, expression.getOperator());
    }
    default boolean isRightAnExpression(Expression<String> expression) {
        final String rhsText = expression.getRightOrDefault("");
        return Operator.getOperatorIn(rhsText, null) != null;
    }

    OPERAND_TYPE parseRight(Expression<String> expression);
}
