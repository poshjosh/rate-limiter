package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.model.Rate;
import io.github.poshjosh.ratelimiter.model.Rates;
import io.github.poshjosh.ratelimiter.util.Ticker;

public interface RateToBandwidthConverter {

    static RateToBandwidthConverter ofDefaults() {
        return ofDefaults(Ticker.ofDefaults());
    }

    static RateToBandwidthConverter ofDefaults(Ticker ticker) {
        return new DefaultRateToBandwidthConverter(ticker);
    }

    Bandwidth convert(Rate rate);

    default Bandwidth convert(Rates rates) {
        return convert(rates, Bandwidths.UNLIMITED);
    }

    Bandwidth convert(Rates rates, Bandwidth resultIfNone);
}
