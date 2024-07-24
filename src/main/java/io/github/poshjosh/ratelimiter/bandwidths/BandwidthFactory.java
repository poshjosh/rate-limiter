package io.github.poshjosh.ratelimiter.bandwidths;

import java.time.Duration;
import java.util.concurrent.TimeUnit;

/**
 * A factory type for creating {@link Bandwidth}s.
 * Implementations are required to have a no-argument constructor.
 */
public interface BandwidthFactory {

    Bandwidth createNew(long permits, long duration, TimeUnit timeUnit, long nowMicros);

    default Bandwidth createNew(long permits, Duration duration) {
        return createNew(permits, duration.toNanos(), TimeUnit.NANOSECONDS);
    }
    default Bandwidth createNew(long permits, Duration duration, long nowMicros) {
        return createNew(permits, duration.toNanos(), TimeUnit.NANOSECONDS, nowMicros);
    }

    default Bandwidth createNew(long permits, long duration, TimeUnit timeUnit) {
        return createNew(permits, duration, timeUnit, 0);
    }
}
