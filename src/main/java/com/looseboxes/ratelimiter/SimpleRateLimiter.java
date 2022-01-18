package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.MapRateCache;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.rates.Logic;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateConfigList;
import com.looseboxes.ratelimiter.util.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class SimpleRateLimiter<K> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(SimpleRateLimiter.class);

    private final RateCache<K, Object> rateCache;

    private final ReadWriteLock cacheLock = new ReentrantReadWriteLock();

    private final RateFactory rateFactory;

    private final Logic logic;

    private final Rate [] limits;

    private final RateRecordedListener rateRecordedListener;

    public SimpleRateLimiter(RateConfig rateConfig) {
        this(new RateConfigList().addLimit(rateConfig));
    }

    public SimpleRateLimiter(Collection<RateConfig> rateConfig) {
        this(new RateConfigList().addLimits(rateConfig));
    }

    public SimpleRateLimiter(RateConfigList rateConfigList) {
        this(new MapRateCache<>(), new LimitWithinDurationFactory(),
                new RateExceededExceptionThrower(), rateConfigList);
    }

    public SimpleRateLimiter(RateCache<K, ?> rateCache, RateFactory rateFactory,
                             RateRecordedListener rateRecordedListener, RateConfigList rateConfigList) {
        this(rateCache, rateFactory, rateRecordedListener, rateConfigList.getLogic(), rateConfigList
                .toRateList().toArray(new Rate[0]));
    }

    public SimpleRateLimiter(RateCache<K, ?> rateCache, RateFactory rateFactory,
                             RateRecordedListener rateRecordedListener, Logic logic, Rate [] limits) {
        this.rateCache = (RateCache<K, Object>)Objects.requireNonNull(rateCache);
        this.rateFactory = Objects.requireNonNull(rateFactory);
        this.rateRecordedListener = Objects.requireNonNull(rateRecordedListener);
        this.logic = Objects.requireNonNull(logic);
        this.limits = new Rate[limits.length];
        System.arraycopy(limits, 0, this.limits, 0, limits.length);
    }

    public SimpleRateLimiter<K> withRateCache(RateCache<K, ?> rateCache) {
        return new SimpleRateLimiter<>(rateCache, this.rateFactory, this.rateRecordedListener, this.logic, this.limits);
    }

    public SimpleRateLimiter<K> withRateFactory(RateFactory rateFactory) {
        return new SimpleRateLimiter<>(this.rateCache, rateFactory, this.rateRecordedListener, this.logic, this.limits);
    }

    public SimpleRateLimiter<K> withRateExceededListener(RateRecordedListener rateRecordedListener) {
        return new SimpleRateLimiter<>(this.rateCache, this.rateFactory, rateRecordedListener, this.logic, this.limits);
    }

    public SimpleRateLimiter<K> withRateLimitConfig(RateConfigList rateConfigList) {
        return new SimpleRateLimiter<>(this.rateCache, this.rateFactory, rateRecordedListener,
                rateConfigList);
    }

    @Override
    public boolean increment(K key, int amount) {

        final Rate existingRate = getRateFromCache(key);

        final Rate next = existingRate == null ? newInitialRate() : existingRate.increment(amount);

        int resetCount = 0;
        int failCount = 0;
        List<Rate> exceededLimits = null; // Initialize only when needed

        for(Rate limit : limits) {

            final int n = next.compareTo(limit);

            if(n == 0) {

                ++resetCount;

            }else if(n > 0) {

                ++failCount;

                if(exceededLimits == null) {
                    exceededLimits = new ArrayList<>(limits.length);
                }
                exceededLimits.add(limit);
            }
        }

        if(exceededLimits == null){
            exceededLimits = Collections.emptyList();
        }

        if(LOG.isDebugEnabled()) {
            LOG.debug("For: {}, limit exceeded: {}, rate: {}, exceeded limits: {}, all limits: {}",
                    key, !exceededLimits.isEmpty(), next, exceededLimits, limits);
        }

        final Rate result = shouldReset(resetCount) ? newInitialRate() : next;
        if(existingRate != result) {
            final boolean putOnlyIfAbsent = existingRate == null;
            addRateToCache(key, result, putOnlyIfAbsent);
        }

        rateRecordedListener.onRateRecorded(this, key, amount, exceededLimits);

        if(Util.isLimitExceeded(failCount, logic, limits)) {
            rateRecordedListener.onRateExceeded(this, key, amount, exceededLimits);
            return false;
        }else{
            return true;
        }
    }

    private boolean shouldReset(int resetCount) {
        return Util.isLimitExceeded(resetCount, logic, limits);
    }

    private Rate getRateFromCache(K key) {
        try{
            cacheLock.readLock().lock();
            return (Rate)rateCache.get(key);
        }finally {
            cacheLock.readLock().unlock();
        }
    }

    private void addRateToCache(K key, Rate rate, boolean onlyIfAbsent) {
        try {
            cacheLock.writeLock().lock();
            if (onlyIfAbsent) {
                // This should mitigate different threads attempting to put a new rate, at the same time
                rateCache.putIfAbsent(key, rate);
            } else {
                rateCache.put(key, rate);
            }
        }finally {
            cacheLock.writeLock().unlock();
        }
    }

    protected Rate newInitialRate() {
        return Objects.requireNonNull(rateFactory.createNew());
    }

    public Logic getLogic() {
        return logic;
    }

    public Rate[] getLimits() {
        return limits;
    }

    public RateCache<K, ?> getRateCache() {
        return rateCache;
    }

    public RateFactory getRateFactory() {
        return rateFactory;
    }

    public RateRecordedListener getRateExceededListener() {
        return rateRecordedListener;
    }

    @Override
    public String toString() {
        return "SimpleRateLimiter@" + Integer.toHexString(hashCode()) + "{logic=" + logic + ", limits=" + Arrays.toString(limits) + '}';
    }
}
