package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.util.Operator;

public class ClassWithMethodAnnotations {
    public class MethodGroupOnlyAnon {
        @RateLimit(limit = 2, duration = 20)
        @RateLimit(limit = 1, duration = 10)
        void anon() { }
    }

    @RateLimitGroup(name = "Fire", logic = Operator.AND)
    @RateLimit(limit = 2, duration = 20)
    @RateLimit(limit = 1, duration = 10)
    void fire() { }
}
