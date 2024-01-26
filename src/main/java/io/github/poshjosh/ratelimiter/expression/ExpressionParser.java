package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.StringUtils;

import java.time.LocalDateTime;

/**
 * Parses expression of one type to another type
 * @param <C> The type of the context e.g a web request (web) or a system (sys) etc
 * @param <T> The type of the resulting expression
 */
public interface ExpressionParser<C, T> {

    static <C> ExpressionParser<C, Long> ofJvmMemory() {
        return new JvmMemoryExpressionParser<>();
    }

    static <C> ExpressionParser<C, LocalDateTime> ofSystemTime() {
        return new SystemTimeExpressionParser<>();
    }

    static <C> ExpressionParser<C, Long> ofSystemTimeElapsed() {
        return new SystemTimeElapsedExpressionParser<>();
    }

    static <C> ExpressionParser<C, String> ofSystemProperty() {
        return new SystemPropertyExpressionParser<>();
    }


    static <C> ExpressionParser<C, String> ofSystemEnvironment() {
        return new SystemEnvironmentExpressionParser<>();
    }

    static <C> ExpressionParser<C, Object> ofJvmThread() {
        return new JvmThreadExpressionParser<>();
    }

    /**
     * @param expression the expression to check if supported
     * @return true if the provided expression is supported
     * @see #isSupported(Expression <String>)
     */
    default boolean isSupported(String expression) {
        return isSupported(Expression.of(expression));
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
    default Expression<T> parse(C context, Expression<String> expression) {
        return Expression.of(
                parseLeft(context, expression), parseOperator(expression), parseRight(expression));
    }

    T parseLeft(C context, Expression<String> expression);

    /**
     * Parse the operator from the expression
     * For input: "sys.environment={service.instances>1}", output operator is ">" not "="
     * @param expression The expression whose operator will be returned
     * @return The operator of the expression
     */
    default Operator parseOperator(Expression<String> expression) {
        final String rhsText = expression.getRightOrDefault("");
        if (StringUtils.hasText(rhsText)) {
            final Operator [] operators = Operator.valuesInMatchSuitableOrder();
            for(Operator operator : operators) {
                if (rhsText.contains(operator.getSymbol())) {
                    return operator;
                }
            }
        }
        return expression.getOperator();
    }
    default boolean isRightAnExpression(Expression<String> expression) {
        final String rhsText = expression.getRightOrDefault("");
        if (StringUtils.hasText(rhsText)) {
            final Operator [] operators = Operator.valuesInMatchSuitableOrder();
            for(Operator operator : operators) {
                if (rhsText.contains(operator.getSymbol())) {
                    return true;
                }
            }
        }
        return false;
    }

    T parseRight(Expression<String> expression);
}
