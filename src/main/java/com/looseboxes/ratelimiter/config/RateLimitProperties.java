package com.looseboxes.ratelimiter.config;

import com.looseboxes.ratelimiter.rates.Rate;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class RateLimitProperties {

    private String controllerPackage;

    private Boolean disabled;

    private Map<String, RateLimitConfig> rateLimits;

    public Map<String, Rate> toRates() {
        final Map<String, Rate> rateMap;
        if(Boolean.TRUE.equals(disabled)) {
            rateMap = Collections.emptyMap();
        }else if(rateLimits == null || rateLimits.isEmpty()) {
            rateMap = Collections.emptyMap();
        }else {
            Map<String, Rate> temp = new HashMap<>(rateLimits.size());
            rateLimits.forEach((name, rateLimitConfig) -> {
                temp.put(name, rateLimitConfig.toRate());
            });
            rateMap = Collections.unmodifiableMap(temp);
        }
        return rateMap;
    }

    public String getControllerPackage() {
        return controllerPackage;
    }

    public void setControllerPackage(String controllerPackage) {
        this.controllerPackage = controllerPackage;
    }

    public Boolean getDisabled() {
        return disabled;
    }

    public void setDisabled(Boolean disabled) {
        this.disabled = disabled;
    }

    public Map<String, RateLimitConfig> getRateLimits() {
        return rateLimits;
    }

    public void setRateLimits(Map<String, RateLimitConfig> rateLimits) {
        this.rateLimits = rateLimits;
    }

    @Override
    public String toString() {
        return "RateLimitProperties{" +
                "controllerPackage='" + controllerPackage + '\'' +
                ", disabled=" + disabled +
                ", rateLimits=" + rateLimits +
                '}';
    }
}
