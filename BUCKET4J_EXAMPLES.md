# Bucket4j Examples

### To create a RateLimiter based on bucket4j-jcache.

pom.xml

```xml
        <dependency>
            <groupId>com.github.vladimir-bukhtoyarov</groupId>
            <artifactId>bucket4j-core</artifactId>
            <version>${bucket4j.version}</version>
        </dependency>
        <dependency>
            <groupId>com.github.vladimir-bukhtoyarov</groupId>
            <artifactId>bucket4j-jcache</artifactId>
            <version>${bucket4j.version}</version>
        </dependency>
```

Code

```java
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
        RateConfigList limits = new RateConfigList().addLimit(new RateConfig().limit(1).duration(Duration.ofSeconds(1)));

        return new Bucket4jRateLimiter<>(proxyManager, new RateExceededExceptionThrower(), limits);
    }

    public List<RateLimiter<K>> newInstancesFromAnnotatedClass(Cache<K, GridBucketState> cache, Class<?> annotationSource) {
        return new RateLimiterListBuilder<K>()
                .rateLimiterFactory(new Bucket4jRateLimiterFactory<>(new JCacheProxyManagerProvider()))
                .rateCache(new JavaRateCache<>(cache)) 
                .build(annotationSource);
    }
}
```

### To create a RateLimiter based on bucket4j-hazelcast.

pom.xml

```xml
        <dependency>
            <groupId>com.github.vladimir-bukhtoyarov</groupId>
            <artifactId>bucket4j-core</artifactId>
            <version>${bucket4j.version}</version>
        </dependency>
        <dependency>
            <groupId>com.hazelcast</groupId>
            <artifactId>hazelcast</artifactId>
            <version>4.0.2</version>
        </dependency>
        <dependency>
            <groupId>com.github.vladimir-bukhtoyarov</groupId>
            <artifactId>bucket4j-hazelcast</artifactId>
            <version>${bucket4j.version}</version>
        </dependency>
```

Code

```java
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
        RateConfigList limits = new RateConfigList().addLimit(new RateConfig().limit(1).duration(Duration.ofSeconds(1)));

        return new Bucket4jRateLimiter<>(proxyManager, new RateExceededExceptionThrower(), limits);
    }

    public List<RateLimiter<K>> newInstancesFromAnnotatedClass(IMap<K, GridBucketState> cache, Class<?> annotationSource) {
        return new RateLimiterListBuilder<K>()
                .rateLimiterFactory(new Bucket4jRateLimiterFactory<>(new HazelcastProxyManagerProvider()))
                .rateCache(new MapRateCache<>(cache))
                .build(annotationSource);
    }
}
```

### To create a RateLimiter based on bucket4j-ignite.

```xml
        <dependency>
            <groupId>com.github.vladimir-bukhtoyarov</groupId>
            <artifactId>bucket4j-core</artifactId>
            <version>${bucket4j.version}</version>
        </dependency>
        <dependency>
            <groupId>org.apache.ignite</groupId>
            <artifactId>ignite-core</artifactId>
            <version>2.11.1</version>
        </dependency>
        <dependency>
            <groupId>org.apache.ignite</groupId>
            <artifactId>ignite-indexing</artifactId>
            <version>2.11.1</version>
        </dependency>
        <dependency>
            <groupId>com.github.vladimir-bukhtoyarov</groupId>
            <artifactId>bucket4j-ignite</artifactId>
            <version>${bucket4j.version}</version>
        </dependency>
```

Code

```java
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
```

### To create a RateLimiter based on bucket4j-infinispan.

TODO

### To create a RateLimiter based on bucket4j-coherence.

TODO