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
        return expression.requireLeft().startsWith(lhs) &&
                (Operators.isGloballySupported(expression.getOperator()) ||
                expression.getOperator().equalsIgnoreNegation(Operator.EQUALS));
    }

    @Override
    public String parseLeft(CONTEXT context, Expression<String> expression) {
        final String left = expression.requireLeft();
        if (!left.startsWith(lhs)) {
            throw Checks.notSupported(this, left);
        }
        final String name = Expressions.getTextInSquareBracketsOrNull(left);
        if (name == null) {
            throw Checks.notSupported(this, left);
        }
        return getValue(name);
    }

    @Override
    public String parseRight(Expression<String> expression) {
        final String right = expression.getRightOrDefault(null);
        return right == null || right.isEmpty() ? null : right;
    }
}
