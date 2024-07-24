package io.github.poshjosh.ratelimiter;

import io.github.poshjosh.ratelimiter.bandwidths.Bandwidth;
import io.github.poshjosh.ratelimiter.bandwidths.Bandwidths;
import io.github.poshjosh.ratelimiter.util.Operator;
import io.github.poshjosh.ratelimiter.util.Ticker;
import io.github.poshjosh.ratelimiter.util.Tickers;

public interface RateLimiters {

    RateLimiter NO_LIMIT = of(Bandwidths.UNLIMITED);

    static RateLimiter of(RateLimiter... rateLimiters) {
        return new RateLimiterArray(rateLimiters);
    }

    static RateLimiter of(Bandwidth... bandwidths) {
        return of(Bandwidths.of(bandwidths));
    }

    static RateLimiter of(Operator operator, Bandwidth... bandwidths) {
        return of(Bandwidths.of(operator, bandwidths));
    }

    static RateLimiter of(Bandwidth bandwidth) {
        return of(bandwidth, Tickers.ofDefaults());
    }

    static RateLimiter of(Bandwidth bandwidth, Ticker ticker) {
        return new DefaultRateLimiter(bandwidth, ticker);
    }
}
