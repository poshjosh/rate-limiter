Remove all System.out.println and any print() methods

# TODO

Fix errors in rate-limiter-javaee and rate-limiter-spring
Do all TODOs
Remove all @since
Fix or record bug: com.looseboxes.ratelimiter.web.javaee.uri.MethodLevelPathPatternsTest - L37,40
   The bug results to failing tests which have been marked with TODO
Publish rate-limiter and associated repositories (to github, maven central?)
Switch to nano precision. A natural unit of cost is "bytes", and a micro precision
  would mean a maximum rate of "1MB/s", which might be small in some cases.

- Breakup WebRequestRateLimiterConfig.Builder into smaller builders for constituent objects e.g DefaultRegistries
- See com.oracle.coherence.common.util.Bandwidth
- Research and add @Inherited to all annotations? This needs some thinking
- Make the entire thing modular, so some libraries are added as needed.
  Matcher related logic e.g MatcherRegistry, should be in its own library.
- Create a branch to try getRateCache(Class keyType, Class valueType)
  Start from DefaultRateLimiter and Bucket4jRateLimiter  
- For README, implement `ProxyManagerProvider` for other bucket4j extensions.
- Find and treat all TODO


- ||| webstore ||| Document need for stickiness and group-level-stickiness in README

### Tests

- Implement PatternMatchingRateLimiterTest
- Test Bucket4jRateLimiter
- Test (in the core package) that RateLimiter works for different amounts, durations and time units.
- Handle and test case where a class has: 1. no path; 2. empty path pattern; but one or more methods of the class has path patterns.
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

