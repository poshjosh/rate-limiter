package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.Matchers;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

final class ParseAheadExpressionMatcher<INPUT, OPERAND_TYPE> implements ExpressionMatcher<INPUT> {

    private static final Logger LOG = LoggerFactory.getLogger(ParseAheadExpressionMatcher.class);

    private final ExpressionParser<INPUT, OPERAND_TYPE> expressionParser;

    private final ExpressionResolver<OPERAND_TYPE> expressionResolver;

    private final Expression<String> expression;
    private final OPERAND_TYPE rhs;

    ParseAheadExpressionMatcher(
            ExpressionParser<INPUT, OPERAND_TYPE> expressionParser,
            ExpressionResolver<OPERAND_TYPE> expressionResolver,
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
        this.rhs = expressionParser.parseRight(expression);
    }

    @Override
    public String match(INPUT toMatch) {
        final OPERAND_TYPE lhs = expressionParser.parseLeft(toMatch, expression);
        final Operator operator = expression.getOperator();
        final boolean success = expressionResolver.resolve(lhs, operator, rhs);
        if (LOG.isTraceEnabled()) {
            LOG.trace("Result: {}, expression: typed {}, text {}",
                    success, StringExprUtil.buildId(lhs, operator, rhs), expression);
        }
        return success ? StringExprUtil.determineResult(expression, lhs, rhs) : Matchers.NO_MATCH;
    }


    @Override
    public ParseAheadExpressionMatcher<INPUT, OPERAND_TYPE> matcher(Expression<String> expression) {
        return new ParseAheadExpressionMatcher<>(expressionParser, expressionResolver, expression);
    }

    @Override
    public boolean isSupported(Expression<String> expression) {
        return expressionResolver.isSupported(expression.getOperator())
                && expressionParser.isSupported(expression);
    }

    @Override
    public String toString() {
        return "ParseAheadExpressionMatcher{" + "expression=" + expression + '}';
    }
}
