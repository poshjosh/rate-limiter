package io.github.poshjosh.ratelimiter.expression;

public interface Expressions {
    Expression<Object> TRUE = of(null, Operator.EQUALS, null);

    Expression<Object> FALSE = TRUE.flipOperator();

    static Expression<String> of(String expression) {
        return StringExprUtil.toExpression(expression);
    }

    static <T> Expression<T> of(T left, String operator, T right) {
        return of(left, Operators.ofSymbol(operator), right);
    }

    static <T> Expression<T> of(T left, Operator operator, T right) {
        return new DefaultExpression<>(left, operator, right);
    }

    /**
     * Get the text in square brackets (at the end of the provide text).
     * Input: <code>"abc.def.ghi[some-name]"</code>
     * Output: <code>"some-name"</code>
     * @param text The text to parse.
     * @return The text in square brackets or <code>null</code> if none.
     */
    static String getTextInSquareBracketsOrNull(String text) {
        return StringExprUtil.getTextInSquareBracketsOrNull(text);
    }
}
