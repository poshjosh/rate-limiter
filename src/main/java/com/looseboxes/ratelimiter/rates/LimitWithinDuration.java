package com.looseboxes.ratelimiter.rates;

import com.looseboxes.ratelimiter.annotation.RateLimitProcessor;

import java.io.Serializable;
import java.util.Objects;

public final class LimitWithinDuration implements Rate, Serializable {

    private final long limit;
    private final long duration;
    private final long timeCreated;

    public LimitWithinDuration() {
        this(1);
    }

    public LimitWithinDuration(long limit) {
        this(limit, 0);
    }

    public LimitWithinDuration(long limit, long duration) {
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
    public Rate increment(int amount) {
        return new LimitWithinDuration(incrementLimit(amount), incrementDuration());
    }

    private long incrementLimit(int amount) {
        return limit + amount;
    }

    private long incrementDuration() {
        return duration + (System.currentTimeMillis() - timeCreated);
    }

    public long getLimit() {
        return limit;
    }

    public long getDuration() {
        return duration;
    }

    public long getTimeCreated() {
        return timeCreated;
    }

    @Override
    public boolean equals(Object o) {super.toString();
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
        return "LimitWithinDuration@" + Integer.toHexString(hashCode()) + "{limit=" + limit + ", duration=" + duration + '}';
    }
}
