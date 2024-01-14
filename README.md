# rate limiter

__Enterprise ready rate limiter.__

Some implementations:

- [rate-limiter-annotation](https://github.com/poshjosh/rate-limiter-annotation).

- [rate-limiter-web-core](https://github.com/poshjosh/rate-limiter-web-core).

- [rate-limiter-spring](https://github.com/poshjosh/rate-limiter-spring).

- [rate-limiter-javaee](https://github.com/poshjosh/rate-limiter-javaee).

To add a dependency on `rate-limiter` using Maven, use the following:

```xml
        <dependency>
            <groupId>io.github.poshjosh</groupId>
            <artifactId>rate-limiter</artifactId>
            <version>0.7.0</version>
        </dependency>
```

### Usage

We want rate limiting to be adjusted dynamically based on specific conditions. [See rate condition expression language](docs/RATE-CONDITION-EXPRESSION-LANGUAGE.md)

```java
import io.github.poshjosh.ratelimiter.bandwidths.Bandwidths;
import io.github.poshjosh.ratelimiter.util.Matcher;

class DynamicRateLimiting {

    static RateLimiter rateLimiter = RateLimiter.of(Bandwidths.ofSeconds(1));

    static Matcher matchAfter2Seconds = Matcher.ofExpression("sys.time.elapsed>PT2S");

    public static void main(String[] args) throws Exception {

        final long startTime = System.currentTimeMillis();

        for (int i = 0; i < 5; i++) {

            final boolean matches = matchAfter2Seconds.matches(startTime);
            System.out.println("Matches: " + matchesCondition + ", elapsed time: " + elapsedTime);

            if (matchesCondition) {
                final boolean acquired = rateLimiter.tryAcquire(1);
                System.out.println("Within limit: " + acquired);
            }

            Thread.sleep(500);
        }
    }
}

// Output
//        Matches: false, elapsed time: 0
//        Matches: false, elapsed time: 504
//        Matches: true, elapsed time: 1004
//        Within limit: true
//        Matches: true, elapsed time: 1505
//        Within limit: true
//        Matches: true, elapsed time: 2008
//        Within limit: false
```

We have a flexible [expression language](docs/RATE-CONDITION-EXPRESSION-LANGUAGE.md) for example:

```
jvm.memory.available<500MB && sys.time.elapsed>PT1S
```

We have a list of tasks to execute, but we don't want to submit more than 2 per second.

```java
class Throttling {
  final RateLimiter rateLimiter = RateLimiter.of(Bandwidth.bursty(2.0)); // 2 permits per second
  void submitTasks(List<Runnable> tasks, Executor executor) {
    for (Runnable task : tasks) {
      rateLimiter.acquire(); // may wait
      executor.execute(task);
    }
  }
}
```

We produce a stream of data, and we want to cap it at 5kb per second. This could be accomplished 
by requiring a permit per byte, and specifying a rate of 5000 permits per second.

```java
class DataCap {
  final RateLimiter rateLimiter = RateLimiter.of(Bandwidth.bursty(5000.0)); // 5000 permits per second
  void submitPacket(byte[] packet) {
    rateLimiter.acquire(packet.length);
    networkService.send(packet);
  }
}
```

### Performance

[Performance statistics](docs/PERFORMANCE.md)

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

### Dependents

The following depend on this library:

- [rate-limiter-annotation](https://github.com/poshjosh/rate-limiter-annotation).

- [rate-limiter-web-core](https://github.com/poshjosh/rate-limiter-web-core).

- [rate-limiter-spring](https://github.com/poshjosh/rate-limiter-spring).

- [rate-limiter-javaee](https://github.com/poshjosh/rate-limiter-javaee).
