package com.looseboxes.ratelimiter.rates;

import com.looseboxes.ratelimiter.annotation.RateLimitProcessor;

import java.io.Serializable;
import java.util.Objects;

public final class AmountPerDuration implements Rate, Serializable {

    private static final long serialVersionUID = 9081726354000000001L;

    public static AmountPerDuration of(long amount, long duration) {
        return new AmountPerDuration(amount, duration, System.currentTimeMillis());
    }

    private final long amount;
    private final long duration;
    private final long timeCreated;

    private AmountPerDuration(long amount, long duration, long timeCreated) {
        final String limitError = RateLimitProcessor.getErrorMessageIfInvalidLimit(amount, null);
        if(limitError != null) {
            throw new IllegalArgumentException(limitError);
        }
        final String durationError = RateLimitProcessor.getErrorMessageIfInvalidDuration(duration, null);
        if(durationError != null) {
            throw new IllegalArgumentException(durationError);
        }
        this.amount = amount;
        this.duration = duration;
        this.timeCreated = timeCreated;
    }

    @Override
    public int compareTo(Rate other) {
        AmountPerDuration amountPerDuration = (AmountPerDuration) other;
        if(amount == amountPerDuration.amount && duration == amountPerDuration.duration) {
            return 0;
        }
        if(amount > amountPerDuration.amount) {
            return duration > amountPerDuration.duration ? 0 : 1;
        }else{
            return -1;
        }
    }

    @Override
    public Rate increment(int amount) {
        return AmountPerDuration.of(incrementLimit(amount), incrementDuration());
    }

    private long incrementLimit(int amount) {
        return this.amount + amount;
    }

    private long incrementDuration() {
        return duration + (System.currentTimeMillis() - timeCreated);
    }

    public long getAmount() {
        return amount;
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
        AmountPerDuration that = (AmountPerDuration) o;
        return amount == that.amount && duration == that.duration;
    }

    @Override
    public int hashCode() {
        return Objects.hash(amount, duration);
    }

    @Override
    public String toString() {
        return "AmountPerDuration@" + Integer.toHexString(hashCode()) +
                "{limit=" + amount + ", duration=" + duration + '}';
    }
}
