package io.github.poshjosh.ratelimiter.expression;

import java.util.Arrays;
import java.util.Objects;

final class AnyExpressionParser<CONTEXT, OPERAND> implements ExpressionParser<CONTEXT, OPERAND>{

    private final ExpressionParser<CONTEXT, OPERAND>[] expressionParsers;

    AnyExpressionParser(ExpressionParser<CONTEXT, OPERAND>[] expressionParsers) {
        this.expressionParsers = Objects.requireNonNull(expressionParsers);
    }

    @Override
    public OPERAND parseLeft(CONTEXT context, Expression<String> expression) {
        for (ExpressionParser<CONTEXT, OPERAND> parser : expressionParsers) {
            if (parser.isSupported(expression)) {
                return parser.parseLeft(context, expression);
            }
        }
        throw Checks.notSupported(this, expression);
    }

    @Override
    public OPERAND parseRight(Expression<String> expression) {
        for (ExpressionParser<CONTEXT, OPERAND> parser : expressionParsers) {
            if (parser.isSupported(expression)) {
                return parser.parseRight(expression);
            }
        }
        throw Checks.notSupported(this, expression);
    }

    @Override
    public boolean isSupported(Expression<String> expression) {
        for (ExpressionParser<CONTEXT, OPERAND> parser : expressionParsers) {
            if (parser.isSupported(expression)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public String toString() {
        return "AnyExpressionParser{parsers=" + Arrays.toString(expressionParsers) + '}';
    }
}
