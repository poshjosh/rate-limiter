package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.Matchers;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

final class ParseAtMatchTimeExpressionMatcher<INPUT, EXPRESSION_TYPE> implements ExpressionMatcher<INPUT> {

    private static final Logger LOG = LoggerFactory.getLogger(ParseAtMatchTimeExpressionMatcher.class);

    private final ExpressionParser<INPUT, EXPRESSION_TYPE> expressionParser;

    private final ExpressionResolver<EXPRESSION_TYPE> expressionResolver;

    private final Expression<String> expression;
    ParseAtMatchTimeExpressionMatcher(
            ExpressionParser<INPUT, EXPRESSION_TYPE> expressionParser,
            ExpressionResolver<EXPRESSION_TYPE> expressionResolver,
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
    public String match(INPUT toMatch) {
        final Expression<EXPRESSION_TYPE> typedExpression = expressionParser.parse(toMatch, expression);
        final boolean success = expressionResolver.resolve(typedExpression);
        if (LOG.isTraceEnabled()) {
            LOG.trace("Success: {}, expression: typed {}, text {}",
                    success, typedExpression, expression);
        }
        return success ?
                StringExprUtil.determineResult(expression, typedExpression) : Matchers.NO_MATCH;
    }

    @Override
    public ParseAtMatchTimeExpressionMatcher<INPUT, EXPRESSION_TYPE> matcher(Expression<String> expression) {
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
