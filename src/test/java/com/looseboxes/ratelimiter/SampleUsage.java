package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.annotation.ClassAnnotationCollector;
import com.looseboxes.ratelimiter.annotation.ClassAnnotationProcessor;
import com.looseboxes.ratelimiter.annotation.RateLimit;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateLimitConfig;
import com.looseboxes.ratelimiter.util.RateLimitGroupData;

import java.util.concurrent.TimeUnit;

// Limited to 3 invocations every 2 second OR 100 invocations every 1 minute
@RateLimit(limit = 3, duration = 2, timeUnit = TimeUnit.SECONDS)
@RateLimit(limit = 100, duration = 1, timeUnit = TimeUnit.MINUTES)
public class SampleUsage {

    public static void main(String... args) {

        // Only one recording is allowed within a second (for each unique recording key)
        RateConfig rateConfig = new RateConfig().limit(1).duration(1).timeUnit(TimeUnit.SECONDS);

        RateLimiter<Integer> rateLimiter = new DefaultRateLimiter<>(rateConfig);

        // We use numbers as recording keys
        rateLimiter.record(1);
        rateLimiter.record(2);
        rateLimiter.record(3);

        // This will fail, it is the second recording of the number 1
        try {
            rateLimiter.record(1);
        }catch(RateLimitExceededException e) {
            System.err.println(e);
        }

        //////////////////////////////////////////////////////////////////////////
        // Using Annotations - See the annotations at the class declaration above
        //////////////////////////////////////////////////////////////////////////

        RateLimitConfig rateLimitConfig = new ClassAnnotationProcessor()
                .process(SampleUsage.class, new ClassAnnotationCollector())
                .values().stream().findFirst()   // Only one method was annotated
                .map(RateLimitGroupData::getConfig)
                .orElseThrow(() -> new RuntimeException("Failed to extract configuration from annotated class"));

        RateLimiter<Object> rateLimiterForClass = new DefaultRateLimiter<>(rateLimitConfig);

        // Call this method as often as required to record usage
        // Will throw an Exception, when the limit within the duration specified by the annotation, is exceeded.
        for(int i=0; i<4; i++) {
            // Will fail on the fourth invocation (i.e when i == 3)
            System.out.println("Record number: " + i);
            rateLimiterForClass.record(i);
        }
    }
}
