package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.annotations.Experimental;
import io.github.poshjosh.ratelimiter.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

final class StringExprUtil {

    // Max length of operator is 3 for now e.g !<= or !>=
    private static final Pattern operatorPattern =
            Pattern.compile("[" + Pattern.quote("=><^$%!") + "]{1,3}");

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
     * Expression: web.session.id!= results to typed expression: <SESSION_ID_VALUE>!=
     * </p>
     * Here, the typed expression is a more suitable identifier because it contains the actual
     * value of the session ID.
     * <br/><br/>
     * However, there are cases when the typed expression changes with time. For example:
     * <p>
     * Expression: sys.time.elapsed>100, results to typed expression: <TIME_ELAPSED_MILLIS>!=100
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

    public static Expression<String> splitIntoExpression(String value) {
        Matcher m = operatorPattern.matcher(value);
        final String lhs;
        final String operator;
        final String rhs;
        if (m.find()) {
            lhs = value.substring(0, m.start()).trim();
            operator = value.substring(m.start(), m.end()).trim();
            rhs = value.substring(m.end()).trim();
        } else {
            throw Checks.notSupported(Expression.class, value);
        }
        if (operator.isEmpty()) {
            throw Checks.notSupported(Expression.class, value);
        }
        return Expression.ofDefault(lhs, operator, rhs);
    }

    private static int indexOfRightExpression(String expression) {
        int rightExpressionStart = expression.indexOf('{');
        if (rightExpressionStart == -1) {
            rightExpressionStart = expression.indexOf('[');
        }
        return rightExpressionStart;
    }

    // Given input: lhs>>>rhs
    // After parsing we get operator = ">". With this method we check that the char after
    // the operator is not also an operator.
    private static void validateRhs(String rhs, Operator[] operators, String value) {
        if (!StringUtils.hasText(rhs)) {
            return;
        }
        final char ch = rhs.charAt(0);
        for(Operator op: operators) {
            if(op.getSymbol().charAt(0) == ch) {
                throw Checks.notSupported(Expression.class, value);
            }
        }
    }

    /**
     * Input: <code>"jvm.memory.free=2G|sys.time.elapsed=PT9M"</code>
     * Output: <code>["jvm.memory.free=2G", "|", "sys.time.elapsed=PT9M"]</code>
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

    // Stats
    // splitIntoExpression(String)
    // Spent memory: 8084KB, time: 25mils
    // Spent memory: 8084KB, time: 23mils
    // Spent memory: 8084KB, time: 23mils
    // splitIntoExpression(String)
    // Spent memory: 8084KB, time: 19mils
    // Spent memory: 8084KB, time: 19mils
    // Spent memory: 8084KB, time: 19mils
    @Experimental
    static Expression<String> splitIntoExpression2(String expression) {
        final int rightExprStart = indexOfRightExpression(expression);
        final String rightExpression = rightExprStart == -1
                ? null : expression.substring(rightExprStart);
        final String partToCheckForOperator = rightExprStart != -1
                ? expression.substring(0, rightExprStart) : expression;

        final Operator [] operators = Operator.valuesInMatchSuitableOrder();
        for(Operator operator: operators) {

            final String symbol = operator.getSymbol();
            final int operatorStart = partToCheckForOperator.indexOf(symbol);
            if (operatorStart == -1) {
                continue;
            }
            final int operatorEnd = operatorStart + symbol.length();
            final String rhs = rightExpression != null
                    ? rightExpression.trim() : expression.substring(operatorEnd).trim();
            validateRhs(rhs, operators, expression);
            final String lhs = expression.substring(0, operatorStart).trim();
            return Expression.ofDefault(lhs, operator, rhs);
        }
        throw Checks.notSupported(Expression.class, expression);
    }
}
