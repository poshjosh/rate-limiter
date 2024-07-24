package io.github.poshjosh.ratelimiter.expression;

/**
 * Resolves expressions of format [LHS OPERATOR RHS]
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

    default boolean resolve(Expression<T> expression) {
        return resolve(
                expression.getLeftOrDefault(null),
                expression.getOperator(),
                expression.getRightOrDefault(null));
    }

    boolean resolve(T left, Operator operator, T right);

    boolean isSupported(Operator operator);
}
