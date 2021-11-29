package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.Logic;

public class ClassWithMethodAnnotations {
    public class MethodGroupOnlyAnon {
        @RateLimit(limit = 2, duration = 20)
        @RateLimit(limit = 1, duration = 10)
        void anon() { }
    }

    @RateLimitGroup(name = "Fire", logic = Logic.AND)
    @RateLimit(limit = 2, duration = 20)
    @RateLimit(limit = 1, duration = 10)
    void fire() { }
}
