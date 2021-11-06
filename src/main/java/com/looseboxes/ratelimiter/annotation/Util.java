package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.LimitWithinDuration;
import com.looseboxes.ratelimiter.rates.Rate;

class Util {
    static Rate [] createRates(RateLimit[] rateLimitArray) {
        final Rate [] rates = new Rate[rateLimitArray.length];
        for(int i=0; i<rateLimitArray.length; i++) {
            final RateLimit rateLimit = rateLimitArray[i];
            final int limit = rateLimit.limit();
            final long duration = rateLimit.timeUnit().toMillis(rateLimit.duration());
            rates[i] = new LimitWithinDuration(limit, duration);
        }
        return rates;
    }
}
