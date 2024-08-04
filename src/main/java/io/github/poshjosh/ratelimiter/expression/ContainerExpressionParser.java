package io.github.poshjosh.ratelimiter.expression;

final class ContainerExpressionParser<CONTEXT> implements ExpressionParser<CONTEXT, Object> {

    private final ExpressionParser<CONTEXT, Object> delegate;
    ContainerExpressionParser() {
        this.delegate = ExpressionParsers.any(
                ExpressionParsers.ofJvmMemory(), ExpressionParsers.ofSystemTime(),
                ExpressionParsers.ofSystemTimeElapsed(), ExpressionParsers.ofJvmThread(),
                ExpressionParsers.ofSystemProperty(), ExpressionParsers.ofSystemEnvironment());
    }

    @Override
    public boolean isSupported(Expression<String> expression) {
        return expression.getOperator().isType(Operator.Type.CONTAINER);
    }

    @Override
    public Object parseLeft(CONTEXT context, Expression<String> expression) {
        try {
            return delegate.parseLeft(context, expression);
        } catch (UnsupportedOperationException e) {
            return expression.getLeftOrDefault(null);
        }
    }

    @Override
    public Object parseRight(Expression<String> expression) {
        final String rhs = expression.getRightOrDefault(null);
        if (rhs == null || rhs.isEmpty()) {
            return null;
        }
        return StringExprUtil.tryValueFromJavaType(rhs, Object.class);
    }
}

