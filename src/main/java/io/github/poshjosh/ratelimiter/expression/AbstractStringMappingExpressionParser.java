package io.github.poshjosh.ratelimiter.expression;

abstract class AbstractStringMappingExpressionParser<CONTEXT> implements ExpressionParser<CONTEXT, String> {

    AbstractStringMappingExpressionParser() { }

    abstract String getLHS();
    abstract String getValue(String name);

    @Override
    public boolean isSupported(Expression<String> expression) {
        return getLHS().equals(expression.requireLeft()) &&
                expression.getOperator().equalsIgnoreNegation(Operator.EQUALS);
    }

    @Override
    public String parseLeft(CONTEXT context, Expression<String> expression) {
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
