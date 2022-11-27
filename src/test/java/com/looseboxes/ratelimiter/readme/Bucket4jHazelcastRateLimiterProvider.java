package com.looseboxes.ratelimiter.readme;

import com.hazelcast.map.IMap;
import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.bucket4j.Bucket4jRateLimiter;
import com.looseboxes.ratelimiter.bucket4j.Bucket4jRateLimiterFactory;
import com.looseboxes.ratelimiter.bucket4j.ProxyManagerProvider;
import com.looseboxes.ratelimiter.builder.RateLimiterListBuilder;
import com.looseboxes.ratelimiter.cache.MapRateCache;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateConfigList;
import io.github.bucket4j.Bucket4j;
import io.github.bucket4j.grid.GridBucketState;
import io.github.bucket4j.grid.ProxyManager;
import io.github.bucket4j.grid.hazelcast.Hazelcast;

import java.io.Serializable;
import java.time.Duration;
import java.util.List;

public class Bucket4jHazelcastRateLimiterProvider<K extends Serializable>{

    private static class HazelcastProxyManagerProvider implements ProxyManagerProvider{

        @Override
        public <K extends Serializable> ProxyManager<K> getProxyManager(RateCache<K, ?> rateCache) {
            // It is possible to get a com.hazelcast.map.IMap via RateCache#unwrap(Class),
            // only if the RateCache is implemented over an com.hazelcast.map.IMap,
            // e.g using com.looseboxes.ratelimiter.cache.MapRateCache
            return Bucket4j.extension(Hazelcast.class).proxyManagerForMap(rateCache.unwrap(IMap.class));
        }
    }

    public RateLimiter<K> newInstance(IMap<K, GridBucketState> cache) {

        ProxyManager<K> proxyManager = Bucket4j.extension(Hazelcast.class).proxyManagerForMap(cache);

        // Limited to one invocation every second
        RateConfigList limits = new RateConfigList().addLimit(RateConfig.of(1, (Duration.ofSeconds(1))));

        return new Bucket4jRateLimiter<>(proxyManager, new RateExceededExceptionThrower(), limits);
    }

    public List<RateLimiter<K>> newInstancesFromAnnotatedClass(IMap<K, GridBucketState> cache, Class<?> annotationSource) {
        return new RateLimiterListBuilder<K>()
                .rateLimiterFactory(new Bucket4jRateLimiterFactory<>(new HazelcastProxyManagerProvider()))
                .rateCache(new MapRateCache<>(cache))
                .build(annotationSource);
    }
}
