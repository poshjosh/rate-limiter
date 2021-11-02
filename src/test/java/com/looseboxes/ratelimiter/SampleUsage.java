package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.annotation.RateLimit;
import com.looseboxes.ratelimiter.annotation.AnnotatedElementIdProvider;
import com.looseboxes.ratelimiter.annotation.builder.RateLimiterBuilders;
import com.looseboxes.ratelimiter.rates.LimitWithinDuration;
import com.looseboxes.ratelimiter.rates.Rate;

import java.lang.reflect.Method;
import java.util.Map;

public class SampleUsage {

    public static void main(String... args) {

        final Rate first = new LimitWithinDuration();

        final long oneSecond = 1000;

        // Only one recording is allowed within a second (for each unique recording key)
        final Rate limit = new LimitWithinDuration(1, oneSecond);

        final RateLimiter<Integer> rateLimiter = new RateLimiterImpl<>(first, limit);

        // We use numbers as recording keys
        rateLimiter.record(1);
        rateLimiter.record(2);
        rateLimiter.record(3);

        // This will fail, it is the second recording of the number 1
        rateLimiter.record(1);

        ////////////////////////////////////////////////////////
        // Using Annotations - See the rate limited method below
        ////////////////////////////////////////////////////////

        final Class targetClass = SampleUsage.class;

        final String sampleRequestPath = "/sampleRequestPath";

        AnnotatedElementIdProvider<Method, String> annotatedElementIdProvider = method -> sampleRequestPath;

        Map<String, RateLimiter<String>> rateLimiters = RateLimiterBuilders.forAnnotatedMethods(String.class)
                .targetClass(targetClass)
                .requestPathsProvider(annotatedElementIdProvider)
                .build();

        RateLimiter<String> rateLimiterForAnnotatedMethod = rateLimiters.get(sampleRequestPath);

        // Call this method as often as required to record usage
        // Will throw an Exception, when the limit within the duration specified by the annotation, is exceeded.
        rateLimiterForAnnotatedMethod.record(sampleRequestPath);
    }

    @RateLimit(limit = 9, duration = 999)
    public void rateLimitedMethod() {

    }
}
