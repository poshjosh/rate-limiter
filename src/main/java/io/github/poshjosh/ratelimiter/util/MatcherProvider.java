package io.github.poshjosh.ratelimiter.util;

import io.github.poshjosh.ratelimiter.model.RateConfig;

import java.util.List;

public interface MatcherProvider<INPUT> {
    static <INPUT> MatcherProvider<INPUT> ofDefaults() {
        return new DefaultMatcherProvider<>();
    }
    static <INPUT> MatcherProvider<INPUT> ofExpression(Class<INPUT> inputType) {
        return new DefaultMatcherProvider<>();
    }
    Matcher<INPUT> createParentMatcher(RateConfig rateConfig);
    List<Matcher<INPUT>> createChildMatchers(RateConfig rateConfig);
}
