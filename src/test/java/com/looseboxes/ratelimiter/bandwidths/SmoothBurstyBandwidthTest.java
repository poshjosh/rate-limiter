package com.looseboxes.ratelimiter.bandwidths;

public class SmoothBurstyBandwidthTest extends BandwidthTest{

    public Bandwidth getBandwidth(double permitsPerSeconds) {
        return SmoothBandwidth.bursty(permitsPerSeconds, readMicros());
    }
}
