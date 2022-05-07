# TODO

- Investigate why request matcher named 'amoeba' does not get called but one called 'default' gets called
  
- Handle and test case where a class has: 1. no path; 2. empty path pattern; but one or more methods of the class has path patterns.

- Make the entire thing modular, so some libraries are added as needed.
  Matcher related logic e.g MatcherRegistry, should be in its own library.
  
- Research and add @Inherited to all annotations? This needs some thinking
  
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

