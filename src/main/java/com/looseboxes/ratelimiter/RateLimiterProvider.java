package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;

public interface RateLimiterProvider<K> {
    Bandwidths initFrom(Bandwidths bandwidths);
    RateLimiter provideRateLimiter(K key, Bandwidths bandwidths);
}
