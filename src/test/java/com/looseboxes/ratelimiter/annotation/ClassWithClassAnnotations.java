package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.Logic;

public class ClassWithClassAnnotations {

    @RateLimit(limit = 2, duration = 20)
    @RateLimit(limit = 1, duration = 10)
    public class ClassGroupOnlyAnon { }

//    @RateLimitGroup("Fire")
    @RateLimitGroup(name = "Fire", logic = Logic.AND)
    @RateLimit(limit = 2, duration = 20)
    @RateLimit(limit = 1, duration = 10)
    public class ClassGroupOnlyNamedFire { }

    public class ClassWithInternalClass {
        @RateLimit(limit = 2, duration = 20)
        @RateLimit(limit = 1, duration = 10)
        public class InternalClass{ }
    }

    @RateLimitGroup
    public class GroupAnnotationOnly { }

    @RateLimitGroup(name = "Fire", logic = Logic.AND)
    @RateLimit(limit = 2, duration = 20)
    @RateLimit(limit = 1, duration = 10)
    public class SecondClassGroupOnlyNamedFire { }

    @RateLimit(limit = 2, duration = 20)
    @RateLimit(limit = 1, duration = 10)
    class PrivateClass{ }
}
