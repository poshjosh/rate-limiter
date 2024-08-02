package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

final class StringExprUtil {

    private StringExprUtil() { }

    static String without(String value, String prefix, String suffix) {
        if (!StringUtils.hasText(value)) {
            return value;
        }
        if (value.startsWith(prefix)) {
            value = value.substring(prefix.length());
        }
        if (value.endsWith(suffix)) {
            value = value.substring(0, value.length() - suffix.length());
        }
        return value;
    }

    /**
     * Determine an appropriate result for the expression.
     * @param expression The string expression
     * @param typedExpression The typed expression resolved from the string expression.
     * @return An appropriate result.
     * @see #determineResult(Expression, Object, Object)
     */
    static String determineResult(Expression<String> expression, Expression<?> typedExpression) {
        return determineResult(expression, typedExpression.getLeftOrDefault(null),
                typedExpression.getRightOrDefault(null));
    }

    /**
     * Determine an appropriate result for the expression.
     * In certain cases, the ID of the typed expression, rather than the ID of the raw
     * (i.e string) expression, could be used as an identifier. For example:
     * <p>
     * Expression: web.session.id !=  results to typed expression: <SESSION_ID_VALUE>!=
     * </p>
     * Here, the typed expression is a more suitable identifier because it contains the actual
     * value of the session ID.
     * <br/><br/>
     * However, there are cases when the typed expression changes with time. For example:
     * <p>
     * Expression: sys.time.elapsed > 100, results to typed expression: <TIME_ELAPSED_MILLIS>!=100
     * </p>
     * Here, the time elapsed in millis changes with time, hence it is not a suitable identifier.
     *
     * @param expression The string expression
     * @param left The left hand side of the typed expression resolved from the string expression
     * @param right The right hand side of the typed expression resolved from the string expression
     * @return An appropriate result.
     */
    static String determineResult(Expression<String> expression, Object left, Object right) {
        return !StringUtils.hasText(expression.getRightOrDefault("")) ?
                buildId(left, expression.getOperator(), right) : expression.getId();
    }

    static <T> String buildId(T left, Operator operator, T right) {
        // TODO - What if right is null, do we want to have null text as part of the id?
        return "{" + left + operator.getSymbol() + right + "}";
    }

    static Expression<String> toExpression(String expression) {
        int idxOfFirstNonSpaceChar = -1;
        for (int i = 0; i < expression.length(); i++) {
            if (!Character.isSpaceChar(expression.charAt(i))) {
                idxOfFirstNonSpaceChar = i;
                break;
            }
        }
        if (idxOfFirstNonSpaceChar == -1) {
            throw Checks.notSupported(Expression.class, expression);
        }
        final int idxOfSpaceBeforeOptr = expression.indexOf(' ', idxOfFirstNonSpaceChar);
        if (idxOfSpaceBeforeOptr == -1) {
            throw Checks.notSupported(Expression.class, expression);
        }
        final String left = expression.substring(0, idxOfSpaceBeforeOptr).trim();
        int operatorStart = -1;
        for (int i = idxOfSpaceBeforeOptr + 1; i < expression.length(); i++) {
            int ch = expression.charAt(i);
            if (!Character.isSpaceChar(ch)) {
                operatorStart = i;
                break;
            }
        }
        if (operatorStart == -1) {
            throw Checks.notSupported(Expression.class, expression);
        }
        final int idxOfSpaceAfterOptr = expression.indexOf(' ', operatorStart);
        final String operator;
        final String right;
        if (idxOfSpaceAfterOptr == -1) {
            operator = expression.substring(operatorStart).trim();
            right = null;
        } else {
            operator = expression.substring(operatorStart, idxOfSpaceAfterOptr).trim();
            right = expression.substring(idxOfSpaceAfterOptr + 1).trim();
        }
        return Expressions.of(left, operator, right);
    }

    /**
     * Input: <code>"jvm.memory.free = 2G|sys.time.elapsed = PT9M"</code>
     * Output: <code>["jvm.memory.free = 2G", "|", "sys.time.elapsed = PT9M"]</code>
     * @param text The text to split
     * @return The result of splitting the value into expressions and conjunctors.
     */
    static String [] splitIntoExpressionsAndConjunctors(String text) {
        Objects.requireNonNull(text);
        if (!StringUtils.hasText(text)) {
            return new String[]{text};
        }
        boolean opened = false;
        int pivot = -1;
        List<String> result = new ArrayList<>();
        for (int i = 0; i < text.length(); i++) {
            char ch = text.charAt(i);
            if ('[' == ch || '{' == ch) {
                opened = true;
            } else if (']' == ch || '}' == ch) { // TODO - Support nesting
                opened = false;
            }
            if (!opened && ('|' == ch || '&' == ch)) { // TODO - support || and &&
                result.add(text.substring(pivot + 1, i).trim());
                result.add(Character.toString(ch));
                pivot = i;
            }
        }
        if (pivot != -1) {
            result.add(text.substring(pivot + 1).trim());
        }
        return result.isEmpty() ? new String[]{text} : result.toArray(new String[0]);
    }
}
