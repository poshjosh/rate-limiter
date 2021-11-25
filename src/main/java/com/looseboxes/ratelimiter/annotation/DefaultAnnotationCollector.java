package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.Rates;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateLimitConfig;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

public class DefaultAnnotationCollector<S> implements AnnotationCollector<S, Map<RateLimitGroupMembers<S>, RateLimitConfig>>{

    private final Map<String, RateLimitGroupMembers<S>> nameToMembers;
    private final Map<RateLimitGroupMembers<S>, RateLimitConfig> result;
    private final Function<S, String> sourceToNameConverter;

    public DefaultAnnotationCollector(Function<S, String> sourceToNameConverter) {
        this.nameToMembers = new HashMap<>();
        this.result = new HashMap<>();
        this.sourceToNameConverter = Objects.requireNonNull(sourceToNameConverter);
    }

    @Override
    public AnnotationCollector<S, Map<RateLimitGroupMembers<S>, RateLimitConfig>> collect(S source, RateLimitGroup rateLimitGroup) {
        String name = selectFirstValidName(rateLimitGroup.name(), rateLimitGroup.value(), sourceToNameConverter.apply(source));
        RateLimitConfig rateLimitConfig = getOrCreateConfig(name, source);
        requireEqualLogic(rateLimitGroup, rateLimitConfig);
        rateLimitConfig.setLogic(rateLimitGroup.logic());
        return this;
    }

    @Override
    public AnnotationCollector<S, Map<RateLimitGroupMembers<S>, RateLimitConfig>> collect(S source, RateLimit rateLimit) {
        String name = selectFirstValidName(rateLimit.group(), sourceToNameConverter.apply(source));
        RateLimitConfig rateLimitConfig = getOrCreateConfig(name, source);
        RateConfig rateConfig = new RateConfig();
        rateConfig.setLimit(rateLimit.limit());
        rateConfig.setDuration(rateLimit.duration());
        rateConfig.setTimeUnit(rateLimit.timeUnit());
        rateLimitConfig.addLimit(rateConfig);
        return this;
    }

    private RateLimitConfig getOrCreateConfig(String name, S source) {
        RateLimitGroupMembers<S> members = nameToMembers.computeIfAbsent(name, RateLimitGroupMembers::new);
        members.addElement(source);
        if(members.getMembers().size() > 1) {
            throw new AnnotationProcessingException("A RateLimitGroup may not be applied to more than 1 class/method");
        }
        return result.computeIfAbsent(members, k -> new RateLimitConfig());
    }

    private void requireEqualLogic(RateLimitGroup rateLimitGroup, RateLimitConfig rateLimitConfig) {
        Rates.Logic existingLogic = rateLimitConfig.getLogic();
        Rates.Logic currentLogic = rateLimitGroup.logic();
        if(existingLogic != null && !existingLogic.equals(currentLogic)) {
            throw new AnnotationProcessingException(RateLimitGroup.class.getSimpleName() +
                    " was defined more than once, with different values for logic()");
        }
    }

    private String selectFirstValidName(String ...candidates) {
        for(String candidate : candidates) {
            if(candidate != null && !candidate.isEmpty()) {
                return candidate;
            }
        }
        throw new AssertionError("A valid fallback value should be provided");
    }

    public Map<RateLimitGroupMembers<S>, RateLimitConfig> getResult() {
        return result.isEmpty() ? Collections.emptyMap() : Collections.unmodifiableMap(result);
    }
}
