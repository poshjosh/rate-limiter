# Rate Limiter

Light-weight rate limiter library.

Limit how much a method is called, or a key is used within a given duration.

### Sample Usage

```java
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.RateLimiterFromAnnotationsBuilder;
import com.looseboxes.ratelimiter.annotation.RateLimit;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

public class SampleUsage {

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
            rateLimiter.increment(rateLimitedMethodId);
        }
    }

    public static void main(String... args) {

        RateLimiter<Object> rateLimiter = buildRateLimiter(RateLimitedResource.class);

        RateLimitedResource rateLimitedResource = new RateLimitedResource(rateLimiter);

        // This will make the last invocation of the method from within the for loop fail
        final int exceedsLimitByOne = RateLimitedResource.LIMIT + 1;

        for (int i = 0; i < exceedsLimitByOne; i++) {

            rateLimitedResource.rateLimitedMethod();
        }
    }

    private static RateLimiter<Object> buildRateLimiter(Class<?> clazz) {
        return new RateLimiterFromAnnotationsBuilder().build(clazz)
                .getChild(0) // Only one endpoint is rate limited
                .getValueOptional().orElseThrow(RuntimeException::new); // Not expected
    }
}
```

### Concept

```java
import com.looseboxes.ratelimiter.SimpleRateLimiter;
import com.looseboxes.ratelimiter.RateLimitExceededException;
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.util.RateConfig;

import java.util.concurrent.TimeUnit;

public class Concept {

    public static void main(String... args) {

        // Only one recording is allowed within a minute (for each unique recording key)
        RateConfig rateConfig = new RateConfig().limit(1).duration(1).timeUnit(TimeUnit.MINUTES);

        RateLimiter<Integer> rateLimiter = new SimpleRateLimiter<>(rateConfig);

        // We use numbers as recording keys
        rateLimiter.increment(1);
        rateLimiter.increment(2);
        rateLimiter.increment(3);

        // This will fail, it is the second recording of the number 1
        try {
            rateLimiter.increment(1);
        } catch (RateLimitExceededException e) {
            System.err.println(e);
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
