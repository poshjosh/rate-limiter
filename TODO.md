# TODO

- Based on the next 2 points, do we need a tree structure? Think carefully
  
- For javaee version read this https://www.baeldung.com/jersey-filters-interceptors
  https://stackoverflow.com/questions/40784139/order-of-jax-rs-filter-and-interceptor
  https://stackoverflow.com/questions/26553200/how-do-i-intercept-selective-methods-and-classes-in-jax-rs-in-java-ee-7-complian
  
- For spring version of rate-limiter consider using a WebMvc to add path specific interceptors:
  registry.addInterceptor(new RequestInterceptor()) .addPathPatterns("/**")
  
- Research and add @Inherited to all annotations? This needs some thinking
  
- Create a branch to try getRateCache(Class keyType, Class valueType)
  Start from SimpleRateLimiter and Bucket4jRateLimiter  

- ||| webstore ||| Document need for stickiness and group-level-stickiness in README
  
- Handle and test case where a class has: 1. no path; 2. empty path pattern; but one or more methods of the class has path patterns.

- Add NodeUtil.collectLeafNodes
  
- Test that RateLimiter is Priority(0) in javaee version

- Do we need to have a rate with 2 quantities returned by methods e.g
  `getLimit` and `getDuration` or do we stick to our current `Rate` interface.
  
- We want to be able to pass request (`HttpServletRequest`/`ContainerRequestContext`) to `RateRecordedListener`.
  * We pass the request as the key argument to both `onRateExceeded` and `onRateRecorded`
  * By having access to the request, the user has more flexibility to respond to rate recorded/exceed events.

- Test Bucket4j rate limiters
- Test usage with javax.cache.Cache and other grid implementations.

- Test multiple limits being exceeded
  
- Test `TimeUnit` for `@RateLimit`.

- Multi threaded tests.
  
- For README, implement `ProxyManagerProvider` for other bucket4j extensions.
  
- Test large amount of classes with @RateLimit annotation.

- Test the use of `Rates.Logic.AND` for `RateLimiterImpl`.

- Find and treat all TODO



