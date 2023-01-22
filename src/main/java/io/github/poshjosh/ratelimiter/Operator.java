package io.github.poshjosh.ratelimiter;

import io.github.poshjosh.ratelimiter.bandwidths.Bandwidths;

import java.util.Objects;

/**
 * A logical operator for a group of bandwidths/rates.
 * @see Bandwidths
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

    DEFAULT("");

    private final String symbol;
    private Operator(String symbol) {
        this.symbol = Objects.requireNonNull(symbol);
    }

    public String getSymbol() {
        return symbol;
    }

    public static Operator ofSymbol(String symbol) {
        switch(symbol) {
            case "&": return AND;
            case "|": return OR;
            default: throw Checks.notSupported(Operator.class, "symbol: " + symbol);
        }
    }
}
