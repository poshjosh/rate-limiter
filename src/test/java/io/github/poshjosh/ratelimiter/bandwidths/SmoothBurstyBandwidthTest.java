package io.github.poshjosh.ratelimiter.bandwidths;

public class SmoothBurstyBandwidthTest extends SmoothBandwidthTest {

    public SmoothBandwidth getBandwidth(double permitsPerSeconds) {
        return (SmoothBandwidth)Bandwidth.bursty(permitsPerSeconds, readMicros());
    }
}
