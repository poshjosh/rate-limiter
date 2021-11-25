package com.looseboxes.ratelimiter.util;

import com.looseboxes.ratelimiter.rates.Logic;
import com.looseboxes.ratelimiter.rates.Rate;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class RateLimitConfig {

    private Logic logic = Logic.OR;

    private List<RateConfig> limits;

    public RateLimitConfig() { }

    public RateLimitConfig(RateLimitConfig rateLimitConfig) {
        this.logic = rateLimitConfig.logic;
        this.limits = rateLimitConfig.limits == null ? null : rateLimitConfig.limits.stream()
                .map(RateConfig::new).collect(Collectors.toList());
    }

    public List<Rate> toRateList() {
        if(limits == null || limits.isEmpty()) {
            return Collections.emptyList();
        }else if(limits.size() == 1) {
            return Collections.singletonList(limits.get(0).toRate());
        }else {
            return limits.stream().map(RateConfig::toRate).collect(Collectors.toList());
        }
    }

    public RateLimitConfig addLimits(List<RateConfig> rateConfigs) {
        rateConfigs.forEach(this::addLimit);
        return this;
    }

    public RateLimitConfig addLimit(RateConfig rateConfig) {
        if(limits == null) {
            limits = new ArrayList<>();
        }
        limits.add(rateConfig);
        return this;
    }

    public RateLimitConfig logic(Logic logic) {
        setLogic(logic);
        return this;
    }

    public Logic getLogic() {
        return logic;
    }

    public void setLogic(Logic logic) {
        this.logic = logic;
    }

    public RateLimitConfig limits(List<RateConfig> limits) {
        setLimits(limits);
        return this;
    }

    public List<RateConfig> getLimits() {
        return limits;
    }

    public void setLimits(List<RateConfig> limits) {
        this.limits = limits;
    }

    @Override
    public String toString() {
        return "RateLimitConfig{" +
                "logic=" + logic +
                ", limits=" + limits +
                '}';
    }
}
