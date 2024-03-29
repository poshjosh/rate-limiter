package io.github.poshjosh.ratelimiter.expression;

import java.util.Arrays;
import java.util.Objects;

public final class Operator {
    enum Type {COMPARISON, STRING}
    public static final Operator EQUALS = new Operator("=", Type.COMPARISON, Type.STRING);
    public static final Operator GREATER = new Operator(">", Type.COMPARISON);
    public static final Operator GREATER_OR_EQUALS = new Operator(">=", Type.COMPARISON);
    public static final Operator LESS = new Operator("<", Type.COMPARISON);
    public static final Operator LESS_OR_EQUALS = new Operator("<=", Type.COMPARISON);
    public static final Operator LIKE = new Operator("%", Type.STRING);
    public static final Operator STARTS_WITH = new Operator("^", Type.STRING);
    public static final Operator ENDS_WITH = new Operator("$", Type.STRING);
    public static final Operator NOT_EQUALS = new Operator("!=", Type.COMPARISON);
    public static final Operator NOT_GREATER = new Operator("!>", Type.COMPARISON);
    public static final Operator NOT_GREATER_OR_EQUALS = new Operator("!>=", Type.COMPARISON);
    public static final Operator NOT_LESS = new Operator("!<", Type.COMPARISON);
    public static final Operator NOT_LESS_OR_EQUALS = new Operator("!<=", Type.COMPARISON);
    public static final Operator NOT_LIKE = new Operator("!%", Type.STRING);
    public static final Operator NOT_STARTS_WITH = new Operator("!^", Type.STRING);
    public static final Operator NOT_ENDS_WITH = new Operator("!$", Type.STRING);
    private static final Operator [] VALUES_IN_MATCH_SUITABLE_ORDER = new Operator[]{
            NOT_GREATER_OR_EQUALS, NOT_LESS_OR_EQUALS, NOT_EQUALS, NOT_GREATER, NOT_LESS,
            NOT_LIKE, NOT_STARTS_WITH, NOT_ENDS_WITH,
            LESS_OR_EQUALS, GREATER_OR_EQUALS, EQUALS, GREATER, LESS,
            LIKE, STARTS_WITH, ENDS_WITH
    };
    public static Operator[] values() {
        return VALUES_IN_MATCH_SUITABLE_ORDER;
    }

    /**
     * We want to match in this order.
     * In this order:
     * <ul>
     *     <li>The negations come first: e.g "!>=" comes before ">="</li>
     *     <li>The compound form comes first: e.g: "<=" comes before "="</li>
     * </ul>
     * @return
     */
    static Operator[] valuesInMatchSuitableOrder() {
        return VALUES_IN_MATCH_SUITABLE_ORDER;
    }
    public static Operator of(String symbol) {
        if (symbol.startsWith("!")) {
            return of(symbol.substring(1)).flip();
        }
        switch(symbol) {
            case "=": return EQUALS;
            case ">": return GREATER;
            case ">=": return GREATER_OR_EQUALS;
            case "<": return LESS;
            case "<=": return LESS_OR_EQUALS;
            case "%": return LIKE;
            case "^": return STARTS_WITH;
            case "$": return ENDS_WITH;
            default: throw Checks.notSupported(Operator.class, symbol);
        }
    }
    private final String symbol;
    private final Type [] types;
    private Operator(String symbol, Type... types) {
        this.symbol = Checks.requireContent(symbol);
        this.types = Objects.requireNonNull(types);
    }
    public boolean equalsIgnoreNegation(Operator operator) {
        return isNegation() ? equals(operator.negative()) : equals(operator.positive());
    }
    public boolean isNegation() {
        return symbol.startsWith("!");
    }
    public Operator positive() {
        return !isNegation() ? this : flip();
    }
    public Operator negative() {
        return isNegation() ? this : flip();
    }
    public Operator flip() {
        return new Operator((isNegation() ? symbol.substring(1) : "!" + symbol), types);
    }
    public boolean isType(Type type) {
        for(Type t : this.types) {
            if (t.equals(type)) {
                return true;
            }
        }
        return false;
    }
    public Type [] getTypes() {
        return Arrays.copyOf(types, types.length);
    }
    public String getSymbol() {
        return symbol;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        Operator operator = (Operator) o;
        return symbol.equals(operator.symbol);// && Arrays.equals(types, operator.types);
    }

    @Override
    public int hashCode() {
        return Objects.hash(symbol/**, Arrays.hashCode(types)*/);
    }

    @Override
    public String toString() { return symbol; }
}
