package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.Logic;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateLimitConfig;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class AnnotationCollectorImpl<S> implements AnnotationCollector<S, Map<RateLimitGroupMembers<S>, RateLimitConfig>>{

    private final Map<String, RateLimitGroupMembers<S>> nameToMembers;
    private final Map<RateLimitGroupMembers<S>, RateLimitConfig> result;
    private final IdProvider<S, String> sourceIdProvider;

    public AnnotationCollectorImpl(IdProvider<S, String> sourceIdProvider) {
        this.nameToMembers = new HashMap<>();
        this.result = new HashMap<>();
        this.sourceIdProvider = Objects.requireNonNull(sourceIdProvider);
    }

    @Override
    public AnnotationCollector<S, Map<RateLimitGroupMembers<S>, RateLimitConfig>> collect(S source, RateLimitGroup rateLimitGroup) {
        String name = selectFirstValidName(rateLimitGroup.name(), rateLimitGroup.value(), sourceIdProvider.getId(source));
        RateLimitConfig rateLimitConfig = getOrCreateConfig(name, source);
        requireEqualLogic(rateLimitGroup, rateLimitConfig, name);
        rateLimitConfig.setLogic(rateLimitGroup.logic());
        return this;
    }

    @Override
    public AnnotationCollector<S, Map<RateLimitGroupMembers<S>, RateLimitConfig>> collect(S source, RateLimit rateLimit) {
        String name = selectFirstValidName(rateLimit.group(), sourceIdProvider.getId(source));
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
            throw new AnnotationProcessingException("A " + RateLimitGroup.class.getName() +
                    " may not be applied to more than 1 class/method. Group: " + name + ", applied to: " + members);
        }
        return result.computeIfAbsent(members, k -> newConfigWithoutDefaultLogic());
    }

    private void requireEqualLogic(RateLimitGroup rateLimitGroup, RateLimitConfig rateLimitConfig, String name) {
        Logic existingLogic = rateLimitConfig.getLogic();
        Logic currentLogic = rateLimitGroup.logic();
        if(existingLogic != null && !existingLogic.equals(currentLogic)) {
            RateLimitGroupMembers<S> members = nameToMembers.get(name);
            throw new AnnotationProcessingException(RateLimitGroup.class.getName() +
                    " was defined more than once, with different values for logic. See: " + members);
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
        return result.isEmpty() ? Collections.emptyMap() : Collections.unmodifiableMap(addDefaultLogic(result));
    }

    private RateLimitConfig newConfigWithoutDefaultLogic() {
        return new RateLimitConfig().logic(null);
    }

    private Map<RateLimitGroupMembers<S>, RateLimitConfig> addDefaultLogic(
            Map<RateLimitGroupMembers<S>, RateLimitConfig> configs) {
        configs.values().stream()
                .filter(rateLimitConfig -> rateLimitConfig.getLogic() == null)
                .forEach(rateLimitConfig -> rateLimitConfig.setLogic(Logic.OR));
        return configs;
    }
}
