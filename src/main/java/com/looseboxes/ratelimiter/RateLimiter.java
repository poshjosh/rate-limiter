package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;

public interface RateLimiter<K> {

    RateLimiter NO_OP = key -> Rate.NONE;

    Rate record(K key) throws RateLimitExceededException;
}
