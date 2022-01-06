package com.looseboxes.ratelimiter.readme;

import com.looseboxes.ratelimiter.RateExceededExceptionThrower;
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.bucket4j.Bucket4jRateLimiter;
import com.looseboxes.ratelimiter.bucket4j.Bucket4jRateLimiterFactory;
import com.looseboxes.ratelimiter.bucket4j.ProxyManagerProvider;
import com.looseboxes.ratelimiter.builder.RateLimiterListBuilder;
import com.looseboxes.ratelimiter.cache.JavaRateCache;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateConfigList;
import io.github.bucket4j.Bucket4j;
import io.github.bucket4j.grid.GridBucketState;
import io.github.bucket4j.grid.ProxyManager;
import io.github.bucket4j.grid.ignite.Ignite;
import org.apache.ignite.IgniteCache;

import java.io.Serializable;
import java.time.Duration;
import java.util.List;

public class Bucket4jIgniteRateLimiterProvider<K extends Serializable>{

    private static class IgniteProxyManagerProvider implements ProxyManagerProvider{

        @Override
        public <K extends Serializable> ProxyManager<K> getProxyManager(RateCache<K, ?> rateCache) {
            // It is possible to get a org.apache.ignite.IgniteCache via RateCache#unwrap(Class),
            // only if the RateCache is implemented over an org.apache.ignite.IgniteCache,
            // e.g using com.looseboxes.ratelimiter.cache.JavaRateCache
            return Bucket4j.extension(Ignite.class).proxyManagerForCache(rateCache.unwrap(IgniteCache.class));
        }
    }

    public RateLimiter<K> newInstance(IgniteCache<K, GridBucketState> cache) {

        ProxyManager<K> proxyManager = Bucket4j.extension(Ignite.class).proxyManagerForCache(cache);

        // Limited to one invocation every second
        RateConfigList limits = new RateConfigList().addLimit(new RateConfig().limit(1).duration(Duration.ofSeconds(1)));

        return new Bucket4jRateLimiter<>(proxyManager, new RateExceededExceptionThrower(), limits);
    }

    public List<RateLimiter<K>> newInstancesFromAnnotatedClass(IgniteCache<K, GridBucketState> cache, Class<?> annotationSource) {
        return new RateLimiterListBuilder<K>()
                .rateLimiterFactory(new Bucket4jRateLimiterFactory<>(new IgniteProxyManagerProvider()))
                .rateCache(new JavaRateCache<>(cache))
                .build(annotationSource);
    }
}
