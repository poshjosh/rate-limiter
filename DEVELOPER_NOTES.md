### Why do we currently use a tree structure for our rate limiters?

Rate limiters could be created per method (for javaee within a DynamicFeature) or
per path pattern (for spring using `WebMvcConfigurer` e.g `registry.addInterceptor(new RequestInterceptor()) .addPathPatterns("/**")`)

However, we choose to use our own tree structure for rate limiters to improve flexibility.

We can thus:

- Add path pattern matchers to random points in our tree.

- Create rate limit groups that do not adhere to single-class-contains-one-or-more-methods paradigm. 
  For example a rate limit group could span the methods of multiple classes.
  
### 

Rather than use annotations: RateLimit and RateLimitGroup, we use classes Rate and RateList
This is because we source data for rate limiting instructions from both annotations and properties
Irrespective of the source, we collect the data using Rate and RateList.

### Misc

- Our code is tightly coupled to a speicific type of Rate. One which comprises of limit and duration.
  This is visiable in: Rate, RateList, AbstractAnnotationProcessor and subclasses etc
  * AbstractAnnotationProcessor could have type parameters AnnotationType, GroupAnnotationType
  * AbstractAnnotationProcessor could use a RateLimit annotation directly, rather than RateList
  * Could we re-name the RateLimit annotation, or its values from limit, duration to something more generic

  !!Note!!

  Rather than use annotations: RateLimit and RateLimitGroup, we use classes Rate and RateList
  This is because we source data for rate limiting instructions from both annotations and properties
  Irrespective of the source, we collect the data using Rate and RateList.

Read

- Very good caching: https://medium.com/@sudheer.sandu/distributed-caching-the-only-guide-youll-ever-need-fe152357f912

- https://dzone.com/articles/detailed-explanation-of-guava-ratelimiters-throttl
- https://www.baeldung.com/guava-rate-limiter
- https://devblogs.microsoft.com/dotnet/announcing-rate-limiting-for-dotnet/
- https://blog.frankel.ch/choose-cache/2/

Guava problems

The most recommended one seems to be the guava RateLimiter. It has a straightforward factory method that gives you a rate limiter for a specified rate (permits per second). However, it doesn’t accomodate web APIs very well, as you can’t initilize the RateLimiter with pre-existing number of permits. That means a period of time should elapse before the limiter would allow requests. There’s another issue – if you have less than one permits per second (e.g. if your desired rate limit is “200 requests per hour”), you can pass a fraction (hourlyLimit / secondsInHour), but it still won’t work the way you expect it to, as internally there’s a “maxPermits” field that would cap the number of permits to much less than you want it to. Also, the rate limiter doesn’t allow bursts – you have exactly X permits per second, but you cannot spread them over a long period of time, e.g. have 5 requests in one second, and then no requests for the next few seconds. In fact, all of the above can be solved, but sadly, through hidden fields that you don’t have access to. Multiple feature requests exist for years now, but Guava just doesn’t update the rate limiter, making it much less applicable to API rate-limiting.
Source: https://techblog.bozho.net/basic-api-rate-limiting/

https://blog.getambassador.io/rate-limiting-a-useful-tool-with-distributed-systems-6be2b1a4f5f4