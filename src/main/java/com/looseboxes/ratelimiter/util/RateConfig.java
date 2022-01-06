package com.looseboxes.ratelimiter.util;

import com.looseboxes.ratelimiter.rates.LimitWithinDuration;
import com.looseboxes.ratelimiter.rates.Rate;

import java.time.Duration;

public class RateConfig {

    private long limit;
    private Duration duration;

    public RateConfig() { }

    public RateConfig(RateConfig rateConfig) {
        this.limit(rateConfig.limit).duration(rateConfig.duration);
    }

    public Rate toRate() {
        return new LimitWithinDuration(limit, duration.toMillis());
    }

    public RateConfig limit(long limit) {
        this.setLimit(limit);
        return this;
    }

    public long getLimit() {
        return limit;
    }

    public void setLimit(long limit) {
        this.limit = limit;
    }

    public RateConfig duration(Duration duration) {
        this.setDuration(duration);
        return this;
    }

    public Duration getDuration() {
        return duration;
    }

    public void setDuration(Duration duration) {
        this.duration = duration;
    }

    @Override
    public String toString() {
        return "RateConfig{limit=" + limit + ", duration=" + duration + '}';
    }
}
