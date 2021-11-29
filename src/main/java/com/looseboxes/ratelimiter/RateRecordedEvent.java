package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;

import java.util.Objects;
import java.util.Optional;

public final class RateRecordedEvent {

    /**
     * The source of the event
     */
    private final Object source;

    /**
     * The key to identify this rate's stream/vector
     */
    private final Object key;

    /**
     * The rate recorded by this event
     */
    private final Rate rate;

    /**
     * The rate (limit) which was exceeded
     */
    private final Rate exceededLimit;

    public RateRecordedEvent(Object source, Object key, Rate rate, Rate exceededLimit) {
        this.source = Objects.requireNonNull(source);
        this.key = Objects.requireNonNull(key);
        this.rate = Objects.requireNonNull(rate);
        this.exceededLimit = exceededLimit;
    }

    public Object getSource() {
        return source;
    }

    public Object getKey() {
        return key;
    }

    public Rate getRate() {
        return rate;
    }

    public boolean isLimitExceeded() {
        return exceededLimit != null;
    }

    public Optional<Rate> getExceededLimitOptional() {
        return Optional.ofNullable(exceededLimit);
    }
}
