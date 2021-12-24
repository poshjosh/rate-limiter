# TODO

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



