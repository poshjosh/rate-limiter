package com.looseboxes.ratelimiter.annotation;

public class ClassWithMethodAnnotations {
    public class MethodGroupOnlyAnon {
        @RateLimit(limit = 2, duration = 20)
        @RateLimit(limit = 1, duration = 10)
        void anon() { }
    }

    @RateLimitGroup("Fire")
    @RateLimit(limit = 2, duration = 20)
    @RateLimit(limit = 1, duration = 10)
    void fire() { }
}
