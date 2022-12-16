package com.looseboxes.ratelimiter.readme;

import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.annotation.NodeData;
import com.looseboxes.ratelimiter.bucket4j.Bucket4jRateLimiter;
import com.looseboxes.ratelimiter.bucket4j.Bucket4jRateLimiterFactory;
import com.looseboxes.ratelimiter.bucket4j.ProxyManagerProvider;
import com.looseboxes.ratelimiter.builder.RateLimitersBuilder;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.rates.Rate;
import io.github.bucket4j.Bucket4j;
import io.github.bucket4j.grid.GridBucketState;
import io.github.bucket4j.grid.ProxyManager;
import io.github.bucket4j.grid.ignite.Ignite;
import org.apache.ignite.IgniteCache;

import java.io.Serializable;
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
        return new Bucket4jRateLimiter<>(proxyManager, Rate.of(1, 1000));
    }

    public List<NodeData<RateLimiter<K>>> newInstancesFromAnnotatedClass(IgniteCache<K, GridBucketState> cache, Class<?> annotationSource) {
        return RateLimitersBuilder.<K>list()
                .rateLimiterFactory(new Bucket4jRateLimiterFactory<>(new IgniteProxyManagerProvider()))
                .rateCache(RateCache.of(cache))
                .build(annotationSource);
    }
}
