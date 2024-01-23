package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.Matcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class ParseAheadExpressionMatcher<R, T> extends ParseAtMatchTimeExpressionMatcher<R, T> {

    private static final Logger LOG = LoggerFactory.getLogger(ParseAheadExpressionMatcher.class);

    private final T rhs;

    ParseAheadExpressionMatcher(
            ExpressionParser<R, T> expressionParser,
            ExpressionResolver<T> expressionResolver,
            Expression<String> expression) {
        super(expressionParser, expressionResolver, expression);
        this.rhs = expressionParser.parseRight(expression);
    }

    @Override
    public String match(R request) {
        final Expression<String> expression = getExpression();
        final T lhs = getExpressionParser().parseLeft(request, expression);
        final Operator operator = expression.getOperator();
        final boolean success = getExpressionResolver().resolve(lhs, operator, rhs);
        if (LOG.isTraceEnabled()) {
            LOG.trace("Result: {}, expression: typed {}, text {}",
                    success, StringExprUtil.buildId(lhs, operator, rhs), expression);
        }
        return success ? StringExprUtil.determineResult(expression, lhs, rhs) : Matcher.NO_MATCH;
    }

    @Override
    public String toString() {
        return "ParseAheadExpressionMatcher{" + "expression=" + getExpression() + '}';
    }
}
