# TODO

- Our code is tightly coupled to a speicific type of Rate. One which comprises of limit and duration. 
  This is visiable in: RateConfig, RateConfigList, AbstractAnnotationProcessor and subclasses etc
  * AbstractAnnotationProcessor could have type parameters AnnotationType, GroupAnnotationType
  * AbstractAnnotationProcessor could use a RateLimit annotation directly, rather than RateConfigList
  * Could we re-name the RateLimit annotation, or its values from limit, duration to something more generic
  
  !!Note!! 
  
  Rather than use annotations: RateLimit and RateLimitGroup, we use classes RateConfig and RateConfigList
  This is because we source data for rate limiting instructions from both annotations and properties
  Irrespective of the source, we collect the data using RateConfig and RateConfigList.
  
- Read this: https://devblogs.microsoft.com/dotnet/announcing-rate-limiting-for-dotnet/
- Investigate: com.oracle.coherence.common.util.Bandwidth.Rate

- Test (in the core package) that RateLimiter works for different amounts, durations and time units.  
- Handle and test case where a class has: 1. no path; 2. empty path pattern; but one or more methods of the class has path patterns.

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

