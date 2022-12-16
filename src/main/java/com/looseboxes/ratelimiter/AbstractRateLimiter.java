package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Limit;
import com.looseboxes.ratelimiter.rates.Rate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

abstract class AbstractRateLimiter<K> implements RateLimiter<K>{

    private static final Logger LOG = LoggerFactory.getLogger(AbstractRateLimiter.class);

    private final Limit limit;

    AbstractRateLimiter(Limit limit) {
        this.limit = Objects.requireNonNull(limit);
    }

    protected abstract Rate getRate(K resourceId);

    protected abstract void setRate(K resourceId, Rate rate);

    protected abstract Rate newInitialRate(int amount);

    @Override
    public boolean consume(Object context, K resourceId, int amount) {

        final Rate existingRate = getRate(resourceId);

        final Rate next = existingRate == null ? newInitialRate(amount) : existingRate.increment(amount);

        int resetCount = 0;
        int failCount = 0;
        List<Rate> exceededLimits = null; // Initialize only when needed

        for(Rate rate : limit.getRates()) {

            final int n = next.compareTo(rate);

            if(n == 0) {

                ++resetCount;

            }else if(n > 0) {

                ++failCount;

                if(exceededLimits == null) {
                    exceededLimits = new ArrayList<>(this.limit.getRateCount());
                }
                exceededLimits.add(rate);
            }
        }

        if(exceededLimits == null){
            exceededLimits = Collections.emptyList();
        }

        if(LOG.isTraceEnabled()) {
            LOG.trace("Limit exceeded: {}, for: {}, rate: {}, exceeded limits: {}, all limits: {}",
                    !exceededLimits.isEmpty(), resourceId, next, exceededLimits, limit);
        }

        final Rate result = shouldReset(resetCount) ? newInitialRate(amount) : next;
        if(existingRate != result) {
            setRate(resourceId, result);
        }

        return limit.isExceeded(failCount);
    }

    private boolean shouldReset(int resetCount) {
        return limit.isExceeded(resetCount);
    }

    public Limit getLimit() {
        return limit;
    }

    @Override
    public String toString() {
        return "SimpleRateLimiter@" + Integer.toHexString(hashCode()) + '_' + limit;
    }
}
