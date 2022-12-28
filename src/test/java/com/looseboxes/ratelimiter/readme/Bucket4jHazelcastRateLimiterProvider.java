package com.looseboxes.ratelimiter.readme;

import com.hazelcast.map.IMap;
import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.annotation.RateLimiterFromAnnotationFactory;
import com.looseboxes.ratelimiter.bucket4j.Bucket4jRateLimiter;
import com.looseboxes.ratelimiter.bucket4j.Bucket4jRateLimiterFactory;
import com.looseboxes.ratelimiter.bucket4j.ProxyManagerProvider;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.Rate;
import com.looseboxes.ratelimiter.util.Rates;
import io.github.bucket4j.Bucket4j;
import io.github.bucket4j.grid.GridBucketState;
import io.github.bucket4j.grid.ProxyManager;
import io.github.bucket4j.grid.hazelcast.Hazelcast;

import java.io.Serializable;

public class Bucket4jHazelcastRateLimiterProvider<K extends Serializable>{

    private static class ProxyManagerProviderImpl implements ProxyManagerProvider{
        @Override
        public <K extends Serializable> ProxyManager<K> getProxyManager(RateCache<K, ?> rateCache) {
            // It is possible to get a com.hazelcast.map.IMap via RateCache#unwrap(Class),
            // only if the RateCache is implemented over an com.hazelcast.map.IMap,
            // e.g using com.looseboxes.ratelimiter.cache.MapRateCache
            return Bucket4j.extension(Hazelcast.class).proxyManagerForMap(rateCache.unwrap(IMap.class));
        }
    }

    public RateLimiter<K> newInstance(IMap<K, GridBucketState> cache, Rate... rates) {
        ProxyManager<K> proxyManager = Bucket4j.extension(Hazelcast.class).proxyManagerForMap(cache);
        return new Bucket4jRateLimiter<>(proxyManager, Rates.of(rates));
    }

    public RateLimiter<K> newInstanceFromAnnotatedClasses(IMap<K, GridBucketState> cache, Class<?>... classes) {
        return RateLimiterFromAnnotationFactory.<K, GridBucketState>of()
                .rateLimiterFactory(new Bucket4jRateLimiterFactory<>(new ProxyManagerProviderImpl()))
                .rateLimiterConfig(RateLimiterConfig.<K, GridBucketState>builder().rateCache(RateCache.of(cache)).build())
                .create(classes);
    }
}
