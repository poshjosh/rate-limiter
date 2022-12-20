/api/limit_1_or_5
MethodLevelPathPatterns{pathPatterns=[/api/limit_1_or_5]}

Should RateLimiter have method setRate() ? What about getRate()?.
We think setRate may be an overkill. However, many tests use that method.

SleepingStopwatch should not extend Stopwatch

Method sleepUninterruptibly does not belong.
For method elapsedMicros, we should simply use Stopwatch.elapsed

# TODO

So that we can have methods without resourceId, we will introduce a default global key
Make sure the single argument method, has synced argument between RateLimiterIx and RateLimiter
Let RateLimiter2 implement our original RateLimiter, then test that exhaustively

Remove all System.out.println and any print() methods
From RateLimiterTest move all Rate related tests to own class
RateLimiter2.setRate should have package access and visible for testing.
Implement serialization properly (esp for cache)
Implement VisibleForTesting
Remove all @since
Remove RateLimiter.setRate and add the rate to the constructor of RateLimiter

- Implement PatternMatchingRateLimiterTest
- Test Bucket4jRateLimiter
- Look for all serialVersionUID and implement Serialization  
- Use Guava RateLimiter -> Add caching to guava rateLimiter
- Test (in the core package) that RateLimiter works for different amounts, durations and time units.  
- Handle and test case where a class has: 1. no path; 2. empty path pattern; but one or more methods of the class has path patterns.
- See com.oracle.coherence.common.util.Bandwidth
  
- Research and add @Inherited to all annotations? This needs some thinking

- Make the entire thing modular, so some libraries are added as needed.
  Matcher related logic e.g MatcherRegistry, should be in its own library.
  
- Create a branch to try getRateCache(Class keyType, Class valueType)
  Start from SimpleRateLimiter and Bucket4jRateLimiter  

- ||| webstore ||| Document need for stickiness and group-level-stickiness in README
  
- Add NodeUtil.collectLeafNodes
  
- Do we need to have a rate with 2 quantities returned by methods e.g
  `getLimit` and `getDuration` or do we stick to our current `Rate` interface.
  
- For README, implement `ProxyManagerProvider` for other bucket4j extensions.

- Find and treat all TODO

### Tests

- Test with different values for: bean-discovery-mode="all"
- Test null path pattern at class level, then at one or more method levels.
- Test Large volume of annotations and properties
- Test case where a class has: 1. no path; 2. empty path pattern; but one or more methods of the class has path patterns.
- Test that RateLimiter is Priority(0) in javaee version
- Test Bucket4j rate limiters
- Test usage with javax.cache.Cache and other grid implementations.
- Test multiple limits being exceeded
- Test `TimeUnit` for `@RateLimit`.
- Multi threaded tests.
- Test large amount of classes with @RateLimit annotation.
- Test the use of `Rates.Logic.AND` for `RateLimiterImpl`.

