package io.github.poshjosh.ratelimiter.expression;

final class SystemEnvironmentExpressionParser<CONTEXT>
        extends AbstractStringMappingExpressionParser<CONTEXT> {

    static final String LHS = "sys.environment";

    SystemEnvironmentExpressionParser() {
        super(LHS);
    }

    @Override String getValue(String name) { return System.getenv(name); }
}
