package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;

import java.util.Objects;

public final class RateExceededEvent {

    /**
     * The source of the event
     */
    private final Object source;

    /**
     * The key to identify this rate's stream/vector
     */
    private final Object key;

    /**
     * The rate (limit) which was exceeded
     */
    private final Rate exceededLimit;

    public RateExceededEvent(Object source, Object key, Rate exceededLimit) {
        this.source = Objects.requireNonNull(source);
        this.key = Objects.requireNonNull(key);
        this.exceededLimit = exceededLimit;
    }

    public Object getSource() {
        return source;
    }

    public Object getKey() {
        return key;
    }

    public Rate getExceededLimit() {
        return exceededLimit;
    }
}
