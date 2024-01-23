package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.StringUtils;

import java.time.LocalDateTime;

/**
 * Parses expression of one type to another type
 * @param <S> The type of the source e.g a web request (web) or a system (sys) etc
 * @param <T> The type of the resulting expression
 */
public interface ExpressionParser<S, T> {

    static <S> ExpressionParser<S, Long> ofJvmMemory() {
        return new JvmMemoryExpressionParser<>();
    }

    static <S> ExpressionParser<S, LocalDateTime> ofSystemTime() {
        return new SystemTimeExpressionParser<>();
    }

    static <S> ExpressionParser<S, Long> ofSystemTimeElapsed() {
        return new SystemTimeElapsedExpressionParser<>();
    }

    static <S> ExpressionParser<S, String> ofSystemProperty() {
        return new SystemPropertyExpressionParser<>();
    }


    static <S> ExpressionParser<S, String> ofSystemEnvironment() {
        return new SystemEnvironmentExpressionParser<>();
    }

    static <S> ExpressionParser<S, Object> ofJvmThread() {
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
    default Expression<T> parse(S source, Expression<String> expression) {
        return Expression.of(
                parseLeft(source, expression), parseOperator(expression), parseRight(expression));
    }

    T parseLeft(S source, Expression<String> expression);

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
