package io.github.poshjosh.ratelimiter.bandwidths;

import java.time.Duration;

public class AllOrNothingBandwidthTest extends AbstractBandwidthTest{
    @Override
    protected Bandwidth getBandwidth(double permitsPerSeconds, long nowMicros) {
        if (permitsPerSeconds != (long)permitsPerSeconds) {
            throw new UnsupportedOperationException("Floating point not supported by this implementation");
        }
        return Bandwidth.allOrNothing((long)permitsPerSeconds, Duration.ofSeconds(1), nowMicros);
    }
}
