package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.Logic;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateLimitConfig;
import com.looseboxes.ratelimiter.util.RateLimitGroupData;
import com.looseboxes.ratelimiter.util.RateLimitGroupDataImpl;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class AnnotationCollectorImpl<S> implements AnnotationCollector<S, Map<String, RateLimitGroupData<S>>>{

    private final Map<String, RateLimitGroupDataImpl<S>> result;
    private final IdProvider<S, String> sourceIdProvider;

    public AnnotationCollectorImpl(IdProvider<S, String> sourceIdProvider) {
        this.result = new HashMap<>();
        this.sourceIdProvider = Objects.requireNonNull(sourceIdProvider);
    }

    @Override
    public AnnotationCollector<S, Map<String, RateLimitGroupData<S>>> collect(S source, RateLimitGroup rateLimitGroup) {
        String name = selectFirstValidName(rateLimitGroup.name(), rateLimitGroup.value(), sourceIdProvider.getId(source));
        RateLimitGroupDataImpl<S> rateLimitGroupData = getOrCreateGroup(name);
        rateLimitGroupData.addMember(name, source);
        final Logic logic = rateLimitGroup.logic();
        RateLimitConfig rateLimitConfig = getOrCreateGroupConfig(rateLimitGroupData);
        rateLimitGroupData.requireEqualLogic(rateLimitGroup);
        rateLimitConfig.setLogic(logic);
        return this;
    }

    @Override
    public AnnotationCollector<S, Map<String, RateLimitGroupData<S>>> collect(S source, RateLimit rateLimit) {
        String name = selectFirstValidName(rateLimit.group(), sourceIdProvider.getId(source));
        RateLimitGroupDataImpl<S> rateLimitGroupData = getOrCreateGroup(name);
        rateLimitGroupData.addMember(name, source);
        RateConfig rateConfig = new RateConfig();
        rateConfig.setLimit(rateLimit.limit());
        rateConfig.setDuration(rateLimit.duration());
        rateConfig.setTimeUnit(rateLimit.timeUnit());
        getOrCreateGroupConfig(rateLimitGroupData).addLimit(rateConfig);
        return this;
    }

    private RateLimitGroupDataImpl<S> getOrCreateGroup(String name) {
        return result.computeIfAbsent(name, s -> new RateLimitGroupDataImpl<>());
    }

    private RateLimitConfig getOrCreateGroupConfig(RateLimitGroupDataImpl<S> groupData) {
        RateLimitConfig rateLimitConfig = groupData.getConfig();
        if(rateLimitConfig == null) {
            rateLimitConfig = newConfigWithoutDefaultLogic();
            groupData.setRateLimitConfig(rateLimitConfig);
        }
        return rateLimitConfig;
    }

    private String selectFirstValidName(String ...candidates) {
        for(String candidate : candidates) {
            if(candidate != null && !candidate.isEmpty()) {
                return candidate;
            }
        }
        throw new AssertionError("A valid fallback value should be provided");
    }

    public Map<String, RateLimitGroupData<S>> getResult() {
        return result.isEmpty() ? Collections.emptyMap() : Collections.unmodifiableMap(addDefaultLogic(result));
    }

    private RateLimitConfig newConfigWithoutDefaultLogic() {
        return new RateLimitConfig().logic(null);
    }

    private Map<String, RateLimitGroupDataImpl<S>> addDefaultLogic(
            Map<String, RateLimitGroupDataImpl<S>> configs) {
        configs.values().stream()
                .map(RateLimitGroupData::getConfig)
                .filter(rateLimitConfig -> rateLimitConfig.getLogic() == null)
                .forEach(rateLimitConfig -> rateLimitConfig.setLogic(Logic.OR));
        return configs;
    }
}
