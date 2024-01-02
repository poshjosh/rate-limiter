package io.github.poshjosh.ratelimiter.util;

import io.github.poshjosh.ratelimiter.expression.ExpressionMatcher;
import io.github.poshjosh.ratelimiter.model.RateConfig;
import io.github.poshjosh.ratelimiter.model.Rates;

import java.util.*;
import java.util.stream.Collectors;

final class DefaultMatcherProvider<INPUT> implements MatcherProvider<INPUT> {

    private static final class NodeNameMatcher<I> implements Matcher<I> {
        private final String name;
        private NodeNameMatcher(String name) {
            this.name = name;
        }
        @Override public String match(I target) {
            return Objects.equals(name, target) ? name : Matcher.NO_MATCH;
        }
        @Override public String toString() {
            return "NodeNameMatcher{" + "name='" + name + '\'' + '}';
        }
    }

    private final ExpressionMatcher<INPUT, Object> expressionMatcher;

    DefaultMatcherProvider() {
        expressionMatcher = ExpressionMatcher.ofDefault();
    }

    @Override public Matcher<INPUT> createParentMatcher(RateConfig rateConfig) {
        final String expression = rateConfig.getRates().getRateCondition();
        return createMatcher(rateConfig.getId(), expression);
    }

    @Override public List<Matcher<INPUT>> createChildMatchers(RateConfig rateConfig) {
        return createMatchers(rateConfig.getRates());
    }

    private Matcher<INPUT> createMatcher(String node, String expression) {
        Matcher<INPUT> matcher = new NodeNameMatcher<>(node);
        return createExpressionMatcher(expression).map(matcher::and).orElse(matcher);
    }

    private List<Matcher<INPUT>> createMatchers(Rates rates) {
        return rates.getLimits().stream()
                .map(rate -> createExpressionMatcher(rate.getRateCondition()).orElse(null))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    private Optional<Matcher<INPUT>> createExpressionMatcher(String expression) {
        return expressionMatcher.matcher(expression);
    }

    @Override public String toString() {
        return "DefaultMatcherProvider{expressionMatcher=" + expressionMatcher + '}';
    }
}
