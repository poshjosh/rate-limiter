package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.model.Operator;
import io.github.poshjosh.ratelimiter.util.Matcher;
import io.github.poshjosh.ratelimiter.util.StringUtils;

import java.util.Optional;

public interface ExpressionMatcher<INPUT> extends Matcher<INPUT> {
    @Override String match(INPUT toMatch);

    ExpressionMatcher<INPUT> matcher(Expression<String> expression);

    boolean isSupported(Expression<String> expression);

    default Optional<Matcher<INPUT>> matcher(String text) {
        if (!StringUtils.hasText(text)) {
            return Optional.empty();
        }
        String [] parts = StringExprUtil.splitIntoExpressionsAndConjunctors(text);
        if (parts.length == 0) {
            return Optional.empty();
        }
        Matcher<INPUT> result = null;
        Operator operator = Operator.NONE;
        for (int i = 0; i < parts.length; i++) {
            final String part = parts[i];
            if (i % 2 == 0) {
                final Matcher<INPUT> matcher;
                if (!StringUtils.hasText(part)) {
                    matcher = null;
                } else {
                    Expression<String> expression = Expressions.of(part);
                    if (isSupported(expression)) {
                        matcher = matcher(expression);
                    } else {
                        throw Checks.notSupported(this, "expression: " + part);
                    }
                }
                if (result == null) {
                    result = matcher;
                } else {
                    switch(operator) {
                        case AND:
                            result = result.and(matcher); break;
                        case OR:
                            result = result.or(matcher); break;
                        case NONE:
                        default:
                            throw Checks.notSupported(this, "operator: " + operator);
                    }
                }
            } else {
                operator = Operator.ofSymbol(parts[i]);
            }
        }
        return Optional.ofNullable(result);
    }
}
