# Rate Limiter

Light weight rate limiter library.

Limit how much a method is called, or a key is used within a given duration.

### Sample Usage

```java
import com.looseboxes.ratelimiter.annotation.AnnotationProcessingException;
import com.looseboxes.ratelimiter.annotation.RateLimit;
import com.looseboxes.ratelimiter.util.RateConfig;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

public class SampleUsage {

    public static void main(String... args) {

        //
        // Direct configuration
        //
        // Only one recording is allowed within a minute (for each unique recording key)
        RateConfig rateConfig = new RateConfig().limit(1).duration(1).timeUnit(TimeUnit.MINUTES);

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

        //
        // Using Annotations - See the annotations at the class declaration above
        //
        final Class<?> targetClass = RateLimitedClass.class;
        RateLimiter<Object> rateLimiterForClass = new RateLimiterBuilder()
                .build(targetClass)
                .orElseThrow(() -> new AnnotationProcessingException(
                        "Failed to build rate limiter for " + targetClass));

        RateLimitedClass rateLimitedClass = new RateLimitedClass(rateLimiterForClass);

        for(int i = 0; i < (RateLimitedClass.LIMIT + 1); i++) {

            rateLimitedClass.rateLimitedMethod();
        }
    }

    static class RateLimitedClass{

        static final int LIMIT = 3;

        RateLimiter<Object> rateLimiter;
        String rateLimitedMethodId;

        RateLimitedClass(RateLimiter<Object> rateLimiter) {
            this.rateLimiter = Objects.requireNonNull(rateLimiter);
            this.rateLimitedMethodId = getClass().getName() + ".rateLimitedMethod";
        }

        // Limited to 3 invocations every 2 second OR 100 invocations every 1 minute
        @RateLimit(limit = LIMIT, duration = 2000)
        @RateLimit(limit = 100, duration = 1, timeUnit = TimeUnit.MINUTES)
        void rateLimitedMethod() {
            rateLimiter.record(rateLimitedMethodId);
        }
    }
}

```

### Build

```sh
mvn clean install
```
