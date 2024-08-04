package io.github.poshjosh.ratelimiter.expression;

import java.util.Objects;

public enum Operator {
    //////////////////////////////////////////////////////////////////
    //
    // The values of this Enum must be in this order:
    // 1. The negations come first: e.g "!>=" comes before ">=".
    // 2. The compound form comes first: e.g: "<=" comes before "=".
    //////////////////////////////////////////////////////////////////

    NOT_GREATER_OR_EQUALS("!>=", Type.COMPARISON),
    NOT_LESS_OR_EQUALS("!<=", Type.COMPARISON),
    NOT_EQUALS("!=", Type.COMPARISON),
    NOT_GREATER("!>", Type.COMPARISON),
    NOT_LESS("!<", Type.COMPARISON),
    NOT_LIKE("!%", Type.STRING),
    NOT_STARTS_WITH("!^", Type.STRING),
    NOT_ENDS_WITH("!$", Type.STRING),
    NOT_IN("!in", Type.CONTAINER),
    LESS_OR_EQUALS("<=", Type.COMPARISON),
    GREATER_OR_EQUALS(">=", Type.COMPARISON),
    EQUALS("=", Type.COMPARISON, Type.STRING),
    GREATER(">", Type.COMPARISON),
    LESS("<", Type.COMPARISON),
    LIKE("%", Type.STRING),
    STARTS_WITH("^", Type.STRING),
    ENDS_WITH("$", Type.STRING),
    IN("in", Type.CONTAINER);

    public enum Type {COMPARISON, STRING, CONTAINER}

    private final String symbol;
    private final Type [] types;
    Operator(String symbol, Type... types) {
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
        return Operators.ofFlip(this);
    }
    public boolean isType(Type type) {
        for(Type t : this.types) {
            if (t.equals(type)) {
                return true;
            }
        }
        return false;
    }
    public String getSymbol() {
        return symbol;
    }

    @Override
    public String toString() { return symbol; }
}
