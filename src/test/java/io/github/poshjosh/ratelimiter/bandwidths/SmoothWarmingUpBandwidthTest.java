package io.github.poshjosh.ratelimiter.bandwidths;

public class SmoothWarmingUpBandwidthTest extends SmoothBandwidthTest {

    public SmoothBandwidth getBandwidth(double permitsPerSeconds) {
        return (SmoothBandwidth) Bandwidths.warmingUp(permitsPerSeconds, readMicros(), 5);
    }
}
