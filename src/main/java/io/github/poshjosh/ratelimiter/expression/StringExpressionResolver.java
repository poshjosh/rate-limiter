package io.github.poshjosh.ratelimiter.expression;

import java.util.Objects;

final class StringExpressionResolver extends AbstractExpressionResolver<String>{

    StringExpressionResolver() {}

    @Override 
    protected boolean resolvePositive(String left, Operator operator, String right) {
        if (!"=".equals(operator.getSymbol())) {
            Objects.requireNonNull(left);
            Objects.requireNonNull(right);
        }
        switch (operator.getSymbol()) {
            case "=":
                return Objects.equals(left, right);
            case "^":
                return left.startsWith(right);
            case "$":
                return left.endsWith(right);
            case "%":
                return left.contains(right);
            default:
                throw Checks.notSupported(this, operator);
        }
    }

    @Override
    public boolean isSupported(Operator operator) {
        return operator.isType(Operator.Type.STRING);
    }
}
