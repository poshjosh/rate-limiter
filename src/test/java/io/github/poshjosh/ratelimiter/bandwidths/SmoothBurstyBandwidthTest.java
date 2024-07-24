package io.github.poshjosh.ratelimiter.bandwidths;

public class SmoothBurstyBandwidthTest extends SmoothBandwidthTest {

    public SmoothBandwidth getBandwidth(double permitsPerSeconds) {
        return (SmoothBandwidth) Bandwidths.bursty(permitsPerSeconds, readMicros());
    }
}
