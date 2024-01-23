package io.github.poshjosh.ratelimiter.expression;

abstract class AbstractExpressionResolver<OPERAND> implements ExpressionResolver<OPERAND>{
    AbstractExpressionResolver() {}

    @Override
    public boolean resolve(OPERAND left, Operator operator, OPERAND right) {
        if (!operator.isNegation()) {
            return resolvePositive(left, operator, right);
        }
        return !resolvePositive(left, operator.flip(), right);
    }

    protected abstract boolean resolvePositive(OPERAND left, Operator operator, OPERAND right);
}
