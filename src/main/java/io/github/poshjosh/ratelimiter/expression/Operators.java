package io.github.poshjosh.ratelimiter.expression;

import java.util.EnumMap;
import java.util.Map;
import java.util.Objects;

public final class Operators {
    private static final Map<Operator, Operator> FLIPS = new EnumMap<>(Operator.class);
    static {
        for (Operator op : Operator.values()) {
            String symbol = op.isNegation() ? op.getSymbol().substring(1) : "!" + op.getSymbol();
            Operator flip = Operators.ofSymbol(symbol);
            FLIPS.put(op, flip);
        }
    }
    public static Operator ofFlip(Operator operator) {
        return Objects.requireNonNull(FLIPS.get(operator));
    }

    public static Operator ofSymbol(String symbol) {
        if (symbol == null || symbol.isEmpty()) {
            throw new IllegalArgumentException("Symbol cannot be null or empty. Symbol: " + symbol);
        }
        final Operator [] operators = Operator.values();
        for(Operator operator : operators) {
            if (symbol.equals(operator.getSymbol())) {
                return operator;
            }
        }
        throw Checks.notSupported(Operator.class, symbol);
    }

    public static Operator ofExpression(String expression, Operator resultIfNone) {
        if (expression == null || expression.isEmpty()) {
            return resultIfNone;
        }
        final Operator [] operators = Operator.values();
        for(Operator operator : operators) {
            if (expression.contains(operator.getSymbol())) {
                return operator;
            }
        }
        return resultIfNone;
    }

    public static boolean isGloballySupported(Operator operator) {
        return false;
    }

    private Operators() { }
}
