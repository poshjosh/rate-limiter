package com.looseboxes.ratelimiter.bandwidths;

public class SmoothWarmingUpBandwidthTest extends BandwidthTest{

    public Bandwidth getBandwidth(double permitsPerSeconds) {
        return Bandwidth.warmingUp(permitsPerSeconds, readMicros(), 5);
    }
}
