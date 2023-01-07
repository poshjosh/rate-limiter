package io.github.poshjosh.ratelimiter.bandwidths;

public class SmoothWarmingUpBandwidthTest extends SmoothBandwidthTest {

    public SmoothBandwidth getBandwidth(double permitsPerSeconds) {
        return (SmoothBandwidth)Bandwidth.warmingUp(permitsPerSeconds, readMicros(), 5);
    }
}
