package com.looseboxes.ratelimiter.rates;

import com.looseboxes.ratelimiter.annotation.RateLimitProcessor;

import java.io.Serializable;
import java.util.Objects;

public final class LimitWithinDuration implements Rate, Serializable {

    private final int limit;
    private final long duration;
    private final long timeCreated;

    public LimitWithinDuration() {
        this(1);
    }

    public LimitWithinDuration(int limit) {
        this(limit, 0);
    }

    public LimitWithinDuration(int limit, long duration) {
        final String limitError = RateLimitProcessor.getErrorMessageIfInvalidLimit(limit, null);
        if(limitError != null) {
            throw new IllegalArgumentException(limitError);
        }
        final String durationError = RateLimitProcessor.getErrorMessageIfInvalidDuration(duration, null);
        if(durationError != null) {
            throw new IllegalArgumentException(durationError);
        }
        this.limit = limit;
        this.duration = duration;
        this.timeCreated = System.currentTimeMillis();
    }

    @Override
    public Rate clone() {
        return new LimitWithinDuration(limit, duration);
    }

    @Override
    public int compareTo(Rate other) {
        LimitWithinDuration limitWithinDuration = (LimitWithinDuration) other;
        if(limit == limitWithinDuration.limit && duration == limitWithinDuration.duration) {
            return 0;
        }
        if(limit > limitWithinDuration.limit) {
            if(duration > limitWithinDuration.duration) {
                return 0;
            }else{
                return 1;
            }
        }else{
            return -1;
        }
    }

    @Override
    public Rate increment() {
        return new LimitWithinDuration(incrementLimit(), incrementDuration());
    }

    private int incrementLimit() {
        return limit + 1;
    }

    private long incrementDuration() {
        return duration + (System.currentTimeMillis() - timeCreated);
    }

    public int getLimit() {
        return limit;
    }

    public long getDuration() {
        return duration;
    }

    public long getTimeCreated() {
        return timeCreated;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LimitWithinDuration that = (LimitWithinDuration) o;
        return limit == that.limit && duration == that.duration;
    }

    @Override
    public int hashCode() {
        return Objects.hash(limit, duration);
    }

    @Override
    public String toString() {
        return "LimitWithinDuration{" +
                "limit=" + limit +
                ", duration=" + duration +
                '}';
    }
}
