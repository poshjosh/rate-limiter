package com.looseboxes.ratelimiter.util;

import com.looseboxes.ratelimiter.rates.LimitWithinDuration;
import com.looseboxes.ratelimiter.rates.Rate;

import java.util.concurrent.TimeUnit;

public class RateConfig {

    private int limit;
    private long duration;
    private TimeUnit timeUnit = TimeUnit.MILLISECONDS;

    public RateConfig() { }

    public RateConfig(RateConfig rateConfig) {
        this.limit(rateConfig.limit).duration(rateConfig.duration).timeUnit(rateConfig.timeUnit);
    }

    public Rate toRate() {
        return new LimitWithinDuration(limit, timeUnit.toMillis(duration));
    }

    public RateConfig limit(int limit) {
        this.setLimit(limit);
        return this;
    }

    public int getLimit() {
        return limit;
    }

    public void setLimit(int limit) {
        this.limit = limit;
    }

    public RateConfig duration(long duration) {
        this.setDuration(duration);
        return this;
    }

    public long getDuration() {
        return duration;
    }

    public void setDuration(long duration) {
        this.duration = duration;
    }

    public RateConfig timeUnit(TimeUnit timeUnit) {
        this.setTimeUnit(timeUnit);
        return this;
    }

    public TimeUnit getTimeUnit() {
        return timeUnit;
    }

    public void setTimeUnit(TimeUnit timeUnit) {
        this.timeUnit = timeUnit;
    }

    @Override
    public String toString() {
        return "RateConfig{" +
                "limit=" + limit +
                ", duration=" + duration +
                ", timeUnit=" + timeUnit +
                '}';
    }
}
