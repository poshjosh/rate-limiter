package io.github.poshjosh.ratelimiter.expression;

import java.util.Objects;

abstract class AbstractStringMappingExpressionParser<CONTEXT> implements ExpressionParser<CONTEXT, String> {

    private final String lhs;
    AbstractStringMappingExpressionParser(String lhs) {
        this.lhs = Objects.requireNonNull(lhs);
    }

    abstract String getValue(String name);

    @Override
    public boolean isSupported(Expression<String> expression) {
        return lhs.equals(expression.requireLeft()) &&
                (Operators.isGloballySupported(expression.getOperator()) ||
                expression.getOperator().equalsIgnoreNegation(Operator.EQUALS));
    }

    @Override
    public String parseLeft(CONTEXT context, Expression<String> expression) {
        final String left = expression.requireLeft();
        if (!lhs.equals(left)) {
            throw Checks.notSupported(this, left);
        }
        Expression<String> rhs = expression.requireRightAsExpression();
        String name = rhs.requireLeft();
        return getValue(name);
    }

    @Override
    public String parseRight(Expression<String> expression) {
        if(expression.getRightOrDefault("").isEmpty()) {
            return null;
        }
        Expression<String> rhs = expression.requireRightAsExpression();
        return rhs.getRightOrDefault(null);
    }
}
