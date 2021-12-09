package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;

public interface RateLimiter<K> {

    RateLimiter<Object> NO_OP = key -> Rate.NONE;

    @SuppressWarnings("unchecked")
    static <T> RateLimiter<T> noop() {
        return (RateLimiter<T>)NO_OP;
    }

    Rate record(K key);
}
