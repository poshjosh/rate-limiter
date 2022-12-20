package com.looseboxes.ratelimiter.bandwidths;

public class SmoothWarmingUpBandwidthTest extends BandwidthTest{

    public Bandwidth getBandwidth(double permitsPerSeconds) {
        return SmoothBandwidth.warmingUp(permitsPerSeconds, readMicros(), 5);
    }
}
