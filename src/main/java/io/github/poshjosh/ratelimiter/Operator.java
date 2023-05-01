package io.github.poshjosh.ratelimiter;

import java.util.Objects;

/**
 * A logical operator for composing a group of bandwidths/rates into a single one
 */
public enum Operator {

    /**
     * Fail when all limits fail
     */
    AND("&"),

    /**
     * Fail when any limit fails.
     */
    OR("|"),

    NONE("");

    private final String symbol;

    Operator(String symbol) {
        this.symbol = Objects.requireNonNull(symbol);
    }

    public String getSymbol() {
        return symbol;
    }

    public static Operator ofSymbol(String symbol) {
        switch(symbol) {
            case "&": return AND;
            case "|": return OR;
            case "": return NONE;
            default: throw Checks.notSupported(Operator.class, "symbol: " + symbol);
        }
    }
}
