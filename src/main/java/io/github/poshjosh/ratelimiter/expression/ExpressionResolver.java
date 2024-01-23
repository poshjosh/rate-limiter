package io.github.poshjosh.ratelimiter.expression;

import java.time.LocalDateTime;

/**
 * Resolves expressions of format [LHS OPERATOR RHS]
 *
 * Supported operators are:
 *
 * <pre>
 * =  equals
 * >  greater
 * >= greater or equals
 * <  less
 * <= less or equals
 * ^  starts with
 * $  ends with
 * %  contains
 * !  negates other operators (e.g !=, !>, !$ etc)
 * </pre>
 */
public interface ExpressionResolver<T> {

    static ExpressionResolver<Long> ofLong() { return new LongExpressionResolver(); }
    static ExpressionResolver<Double> ofDecimal() { return new DecimalExpressionResolver(); }
    static ExpressionResolver<String> ofString() { return new StringExpressionResolver(); }
    static ExpressionResolver<LocalDateTime> ofDateTime() { return new DateTimeExpressionResolver(); }
    static ExpressionResolver<Object> ofJvmThread() { return new JvmThreadExpressionResolver(); }

    default boolean resolve(Expression<T> expression) {
        return resolve(
                expression.getLeftOrDefault(null),
                expression.getOperator(),
                expression.getRightOrDefault(null));
    }

    boolean resolve(T left, Operator operator, T right);

    boolean isSupported(Operator operator);
}
