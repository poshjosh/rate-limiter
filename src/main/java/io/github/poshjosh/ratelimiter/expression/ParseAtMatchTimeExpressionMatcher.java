package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.Matcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

final class ParseAtMatchTimeExpressionMatcher<R, T> implements ExpressionMatcher<R, T> {

    private static final Logger LOG = LoggerFactory.getLogger(ParseAtMatchTimeExpressionMatcher.class);

    private final ExpressionParser<R, T> expressionParser;

    private final ExpressionResolver<T> expressionResolver;

    private final Expression<String> expression;
    ParseAtMatchTimeExpressionMatcher(
            ExpressionParser<R, T> expressionParser,
            ExpressionResolver<T> expressionResolver,
            Expression<String> expression) {
        if (!expressionResolver.isSupported(expression.getOperator())) {
            throw Checks.notSupported(expressionResolver,
                    "operator: " + expression.getOperator());
        }
        if (!expressionParser.isSupported(expression)) {
            throw Checks.notSupported(expressionParser, expression);
        }
        this.expressionParser = Objects.requireNonNull(expressionParser);
        this.expressionResolver = Objects.requireNonNull(expressionResolver);
        this.expression = Objects.requireNonNull(expression);
    }

    @Override
    public String match(R request) {
        final Expression<T> typedExpression = expressionParser.parse(request, expression);
        final boolean success = expressionResolver.resolve(typedExpression);
        if (LOG.isTraceEnabled()) {
            LOG.trace("Success: {}, expression: typed {}, text {}",
                    success, typedExpression, expression);
        }
        return success ?
                StringExprUtil.determineResult(expression, typedExpression) : Matcher.NO_MATCH;
    }

    @Override
    public ParseAtMatchTimeExpressionMatcher<R, T> matcher(Expression<String> expression) {
        return new ParseAtMatchTimeExpressionMatcher<>(expressionParser, expressionResolver, expression);
    }

    @Override
    public boolean isSupported(Expression<String> expression) {
        return expressionResolver.isSupported(expression.getOperator())
                && expressionParser.isSupported(expression);
    }

    @Override
    public String toString() {
        return "ParseAtMatchTimeExpressionMatcher{" + "expression=" + expression + '}';
    }
}
