# Rate Limiter

Light-weight rate limiter library.

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
        // Using Annotations - See the class with the rate limited method below
        //
        final Class<?> targetClass = RateLimitedResource.class;
        RateLimiter<Object> rateLimiterForClass = new RateLimiterBuilder()
                .build(targetClass)
                .orElseThrow(() -> new AnnotationProcessingException(
                        "Failed to build rate limiter for " + targetClass));

        RateLimitedResource rateLimitedResource = new RateLimitedResource(rateLimiterForClass);

        // This will make the last invocation of the method from within the for loop fail
        final int exceedsLimitByOne = RateLimitedResource.LIMIT + 1;

        for(int i = 0; i < exceedsLimitByOne; i++) {

            rateLimitedResource.rateLimitedMethod();
        }
    }

    static class RateLimitedResource {

        static final int LIMIT = 3;

        final RateLimiter<Object> rateLimiter;
        final String rateLimitedMethodId;

        RateLimitedResource(RateLimiter<Object> rateLimiter) {
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

### Performance

[See performance statistics](PERFORMANCE.md)

### Build

__This is a maven based java project, so:__

To package a jar file

```sh
mvn clean package
```

To install, thus making it referencable in other project's pom.xm

```sh
mvn clean install
```

Enjoy! :wink:
