package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.StringUtils;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

final class StringExprUtil {
    private static final char OPEN_SQUARE_BRACKET = '[';
    private static final char CLOSE_SQUARE_BRACKET = ']';
    private static final char OPEN_BRACKET = '{';
    private static final char CLOSE_BRACKET = '}';
    private static final char OR = '|';
    private static final char AND = '&';

    private StringExprUtil() { }

    /**
     * Get the text in square brackets (at the end of the provide text).
     * Input: <code>"abc.def.ghi[some-name]"</code>
     * Output: <code>"some-name"</code>
     * @param text The text to parse.
     * @return The text in square brackets or <code>null</code> if none.
     */
    static String getTextInSquareBracketsOrNull(String text) {
        if (text == null || text.isEmpty()) {
            return null;
        }
        if (text.charAt(text.length() - 1) != CLOSE_SQUARE_BRACKET) {
            return null;
        }
        final int indexOfOpenSquareBracket = text.indexOf(OPEN_SQUARE_BRACKET);
        if (indexOfOpenSquareBracket == -1) {
            return null;
        }
        return text.substring(indexOfOpenSquareBracket + 1, text.length() - 1);
    }

    /**
     * Determine a value from specified method or field.
     * Supports only:
     * <ul>
     *     <li>public static methods or fields</li>
     *     <li>methods with no arguments</li>
     * </ul>
     * Expected input format:
     * <code>
     *     [class]#[field|method()]
     * </code>
     * <p>Examples for:</p>
     * <ul>
     *     <li>methods: "io.github.ratelimiter.StringExprUtil#getGreetings()"</li>
     *     <li>fields: "io.github.ratelimiter.StringExprUtil#greeting"</li>
     * </ul>
     * @param text The text to parse
     * @return The value from the determined method or field
     */
    static <T> T tryValueFromJavaType(String text, Class<T> type) {
        if (!StringUtils.hasText(text)) {
            throw invalidTextException(text, null);
        }
        final int classEnd = text.indexOf("#");
        if (classEnd == -1) {
            if (type.isAssignableFrom(String.class)) {
                return (T)text;
            }
            throw invalidTextException(text, null);
        }
        final String className = text.substring(0, classEnd).trim();
        try {
            final Class<?> clazz = Class.forName(className);
            final String name = text.substring(classEnd + 1).trim();
            final boolean isMethod = name.endsWith("()");
            if (isMethod) {
                final Method method = clazz.getMethod(name.substring(0, name.length() - 2));
                return type.cast(method.invoke(null));
            }
            return type.cast(clazz.getField(name).get(null));
        } catch (ClassNotFoundException | NoSuchMethodException | SecurityException |
                 IllegalAccessException | InvocationTargetException | NoSuchFieldException e) {
            if (type.isAssignableFrom(String.class)) {
                return (T)text;
            }
            throw invalidTextException(text, e);
        }
    }

    private static RuntimeException invalidTextException(String text, Throwable cause) {
        return new IllegalArgumentException("Invalid: " + text
                + ". Expected format: <class>#<field|method()> e.g org.spaces.Me#getName()", cause);
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
        return "" + OPEN_BRACKET + left + ' ' + operator.getSymbol() + ' ' + right + CLOSE_BRACKET;
    }

    static Expression<String> toExpression(String expression) {
        final int idxOfFirstNonSpaceChar = indexOfFirstNonSpaceChar(expression, 0);
        final int idxOfSpaceBeforeOptr = expression.indexOf(' ', idxOfFirstNonSpaceChar);
        if (idxOfSpaceBeforeOptr == -1) {
            throw Checks.notSupported(Expression.class, expression);
        }
        final String left = expression.substring(0, idxOfSpaceBeforeOptr).trim();
        final int operatorStart = indexOfFirstNonSpaceChar(expression, idxOfSpaceBeforeOptr + 1);
        final int idxOfSpaceAfterOptr = expression.indexOf(' ', operatorStart);
        final String operatorStr = parseOperator(expression, operatorStart, idxOfSpaceAfterOptr);
        final Operator operator = Operators.ofSymbol(operatorStr); // Ensure operator is valid
        final String right = parseRight(expression, idxOfSpaceAfterOptr);
        return Expressions.of(left, operator, right);
    }

    private static int indexOfFirstNonSpaceChar(String expression, int offset) {
        for (int i = offset; i < expression.length(); i++) {
            if (!Character.isSpaceChar(expression.charAt(i))) {
                return i;
            }
        }
        throw Checks.notSupported(Expression.class, expression);
    }

    private static String parseOperator(String expression, int operatorStart, int idxOfSpaceAfterOptr) {
        if (idxOfSpaceAfterOptr == -1) {
            return expression.substring(operatorStart).trim();
        } else {
            return expression.substring(operatorStart, idxOfSpaceAfterOptr).trim();
        }
    }

    private static String parseRight(String expression, int idxOfSpaceAfterOptr) {
        if (idxOfSpaceAfterOptr == -1) {
            return null;
        } else {
            final int next = idxOfSpaceAfterOptr + 1;
            if (expression.length() < next) {
                throw Checks.notSupported(Expression.class, expression);
            }
            return expression.substring(next).trim();
        }
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
            if (OPEN_SQUARE_BRACKET == ch || OPEN_BRACKET == ch) {
                opened = true;
            } else if (CLOSE_SQUARE_BRACKET == ch || CLOSE_BRACKET == ch) { // TODO - Support nesting
                opened = false;
            }
            if (!opened && (OR == ch || AND == ch)) { // TODO - support || and &&
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
