package io.github.poshjosh.ratelimiter.expression;

final class SystemPropertyExpressionParser<CONTEXT>
        extends AbstractStringMappingExpressionParser<CONTEXT> {

    static final String LHS = "sys.property";

    SystemPropertyExpressionParser() { }

    @Override String getLHS() { return LHS; }

    @Override String getValue(String name) { return System.getProperty(name, null); }
}
