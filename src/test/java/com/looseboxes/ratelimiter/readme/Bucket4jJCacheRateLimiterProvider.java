package com.looseboxes.ratelimiter.readme;

import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.annotation.NodeData;
import com.looseboxes.ratelimiter.bucket4j.Bucket4jRateLimiter;
import com.looseboxes.ratelimiter.bucket4j.Bucket4jRateLimiterFactory;
import com.looseboxes.ratelimiter.bucket4j.ProxyManagerProvider;
import com.looseboxes.ratelimiter.builder.RateLimitersBuilder;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.Rate;
import io.github.bucket4j.Bucket4j;
import io.github.bucket4j.grid.GridBucketState;
import io.github.bucket4j.grid.ProxyManager;
import io.github.bucket4j.grid.jcache.JCache;

import javax.cache.Cache;
import java.io.Serializable;
import java.time.Duration;
import java.util.List;

public class Bucket4jJCacheRateLimiterProvider<K extends Serializable>{

    private static class JCacheProxyManagerProvider implements ProxyManagerProvider{

        @Override
        public <K extends Serializable> ProxyManager<K> getProxyManager(RateCache<K, ?> rateCache) {
            // It is possible to get a javax.cache.Cache via RateCache#unwrap(Class),
            // only if the RateCache is implemented over an javax.cache.Cache,
            // e.g using com.looseboxes.ratelimiter.cache.JavaRateCache
            return Bucket4j.extension(JCache.class).proxyManagerForCache(rateCache.unwrap(Cache.class));
        }
    }

    public RateLimiter<K> newInstance(Cache<K, GridBucketState> cache) {

        ProxyManager<K> proxyManager = Bucket4j.extension(JCache.class).proxyManagerForCache(cache);

        // Limited to one invocation every second
        return new Bucket4jRateLimiter<>(proxyManager, Rate.of(1, Duration.ofSeconds(1)));
    }

    public List<NodeData<RateLimiter<K>>> newInstancesFromAnnotatedClass(Cache<K, GridBucketState> cache, Class<?> annotationSource) {
        return RateLimitersBuilder.<K>list()
                .rateLimiterFactory(new Bucket4jRateLimiterFactory<>(new JCacheProxyManagerProvider()))
                .rateCache(RateCache.of(cache))
                .build(annotationSource);
    }
}
