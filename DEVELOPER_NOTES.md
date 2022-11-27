### Why do we currently use a tree structure for our rate limiters?

Rate limiters could be created per method (for javaee within a DynamicFeature) or
per path pattern (for spring using `WebMvcConfigurer` e.g `registry.addInterceptor(new RequestInterceptor()) .addPathPatterns("/**")`)

However, we choose to use our own tree structure for rate limiters to improve flexibility.

We can thus:

- Add path pattern matchers to random points in our tree.

- Create rate limit groups that do not adhere to single-class-contains-one-or-more-methods paradigm. 
  For example a rate limit group could span the methods of multiple classes.
  
### 

Rather than use annotations: RateLimit and RateLimitGroup, we use classes RateConfig and RateConfigList
This is because we source data for rate limiting instructions from both annotations and properties
Irrespective of the source, we collect the data using RateConfig and RateConfigList.

