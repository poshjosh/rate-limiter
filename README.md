# Rate Limiter

Light-weight rate limiter library.

Limit how much a resource (class/method/key) is used within a given duration.

The aim is to be simple but flexible, for example to limit a method to 3 invocations every 2 seconds:

```java
class RateLimitedResource {

    // Method limited to 3 invocations every 2 seconds
    @RateLimit(limit = 3, duration = 2000)
    String rateLimitedMethod() {
        return "Hello World!";
    }
}
```

A more complex example:

```java
// All methods collectively limited to 120 invocations every 1 minute
@RateLimit(limit = 120, duration = 1, timeUnit = TimeUnit.MINUTES)
class RateLimitedResource {

    // Method limited to 3 invocations every 2 seconds OR 100 invocations every 1 minute
    @RateLimit(limit = 3, duration = 2000)
    @RateLimit(limit = 100, duration = 1, timeUnit = TimeUnit.MINUTES)
    void rateLimitedMethod_1() {
        return "Hello World 1!";
    }

    // Method limited to 3 invocations every 1 second
    @RateLimit(limit = 3, duration = 1000)
    void rateLimitedMethod_2() {
        return "Hello World 2!";
    }
}
```

### Sample Usage

```java
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.builder.RateLimiterListBuilder;
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

        // Limited to 3 invocations every 2 seconds OR 100 invocations every 1 minute
        @RateLimit(limit = LIMIT, duration = 2000) @RateLimit(limit = 100, duration = 1, timeUnit = TimeUnit.MINUTES) void rateLimitedMethod() {

            // VERY IMPORTANT to record usage
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
        return new RateLimiterListBuilder<>().build(clazz).get(0); // Only one class/method is rate limited
    }
}
```

### Bucket4j Examples

[Bucket4j Examples](BUCKET4J_EXAMPLES.md)

### Annotation Specification

- The `@RateLimit` annotation may be placed on a super class.

- A `@RateLimit` annotation at the class level applies to all methods of the class having a
  `@RateLimit` annotation.

- A `@RateLimit` annotation may be assigned to a group using a `@RateLimitGroup` annotation.

- If A `@RateLimitGroup` annotation is not specified the `@RateLimit` annotation, is
  assigned to a default group:

  * At the class level, the group is named after the fully qualified class name.

  * At the method level, the group is named after the fully qualified class name and method signature.

- The `@RateLimitGroup` annotation may span multiple classes or methods but not both.

**Example**

Lets say we have 3 classes `Resource1`, `Resource2` and `Resource3`; rate limited as shown below:

```java
class Resource1{
    
    @RateLimit(limit = 1, duration = 999)
    void methodA() {}

    @RateLimit(limit = 1, duration = 999)
    void methodB() {}

    @RateLimit(limit = 1, duration = 999)
    @RateLimitGroup("method-group")
    void methodC() {}
}
```

```java
@RateLimitGroup("class-group")
class Resource2{
    
    @RateLimit(limit = 1, duration = 999)
    void methodA() {}

    @RateLimit(limit = 1, duration = 999)
    @RateLimitGroup("method-group")
    void methodB() {}

    @RateLimit(limit = 1, duration = 999)
    void methodC() {}
}
```

```java
@RateLimitGroup("class-group")
class Resource3{
    
    @RateLimit(limit = 1, duration = 999)
    void methodA() {}
}
```

**Example Hierarchy**

```
                                              root
                                               |
              -------------------------------------------------------------------
              |                                |                                |    
         class-group                      method-group                          |       
              |                                |                                |                
    ---------------------                      |                                |
    |                   |                      |                                |
Resource2           Resource3                  |                            Resource1
    |                   |                      |                                | 
Resource2#methodA   Resource3#methodA   Resource1#methodC                   Resource1#methodA
Resource2#methodC                       Resource2#methodB                   Resource1#methodB

```

### Concept

```java
import com.looseboxes.ratelimiter.RateExceededException;
import com.looseboxes.ratelimiter.SimpleRateLimiter;
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.util.RateConfig;

import java.util.concurrent.TimeUnit;

public class Concept {

  public static void main(String... args) {

    // Only one recording is allowed within a minute (for each unique recording key)
    RateConfig rateConfig = new RateConfig().limit(1).duration(Duration.ofMinutes(1));

    RateLimiter<Integer> rateLimiter = new SimpleRateLimiter<>(rateConfig);

    // We use numbers as recording keys
    rateLimiter.increment(1);
    rateLimiter.increment(2);
    rateLimiter.increment(3);

    // This will fail, it is the second recording of the number 1
    try {
      rateLimiter.increment(1);
    } catch (RateExceededException e) {
      System.err.println(e);
    }
  }
}
```

### Performance

[Performance statistics](PERFORMANCE.md)

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
