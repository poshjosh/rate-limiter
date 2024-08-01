package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.StringUtils;

import java.util.Locale;

final class JvmMemoryExpressionParser<CONTEXT> implements ExpressionParser<CONTEXT, Long> {

    public static final String MEMORY_AVAILABLE = "jvm.memory.available";
    public static final String MEMORY_FREE = "jvm.memory.free";
    public static final String MEMORY_MAX = "jvm.memory.max";
    public static final String MEMORY_TOTAL = "jvm.memory.total";
    public static final String MEMORY_USED = "jvm.memory.used";

    // b has to come before all others, because all others end with b
    private static final String [] SUFFIXES = {"yb", "zb", "eb", "pb", "tb", "gb", "mb", "kb", "b"};

    JvmMemoryExpressionParser() { }

    @Override
    public boolean isSupported(Expression<String> expression) {
        final String lhs = expression.requireLeft();
        switch (lhs) {
            case MEMORY_AVAILABLE:
            case MEMORY_FREE:
            case MEMORY_MAX:
            case MEMORY_TOTAL:
            case MEMORY_USED:
                return expression.getOperator().isType(Operator.Type.COMPARISON);
            default:
                return false;
        }
    }

    @Override
    public Long parseLeft(CONTEXT context, Expression<String> expression) {
        final String lhs = expression.requireLeft();
        switch (lhs) {
            case MEMORY_AVAILABLE:
                return MemoryUtil.availableMemory();
            case MEMORY_FREE:
                return Runtime.getRuntime().freeMemory();
            case MEMORY_MAX:
                return Runtime.getRuntime().maxMemory();
            case MEMORY_TOTAL:
                return Runtime.getRuntime().totalMemory();
            case MEMORY_USED:
                return MemoryUtil.usedMemory();
            default:
                throw Checks.notSupported(this, lhs);
        }
    }

    @Override
    public Long parseRight(Expression<String> expression) {
        final String rhsText = expression.getRightOrDefault("");
        if (!StringUtils.hasText(rhsText)) {
            return null;
        }
        // This .replace('_', '\u0000') did not yield the desired result in some cases
        final String rhs = rhsText.replace("_", "").toLowerCase(Locale.ROOT);
        for (int i = 0; i < SUFFIXES.length; i++) {
            final int factor = SUFFIXES.length - i - 1;
            if (rhs.endsWith(SUFFIXES[i])) {
                final String numStr = rhs.substring(0, rhs.length() - SUFFIXES[i].length());
                return Long.parseLong(numStr) * ((long)Math.pow(1000, factor));
            }
        }
        return Long.parseLong(rhs);
    }
}
