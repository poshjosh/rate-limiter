package com.looseboxes.ratelimiter.bandwidths;

public class SmoothBurstyBandwidthTest extends BandwidthTest{

    public Bandwidth getBandwidth(double permitsPerSeconds) {
        return Bandwidth.bursty(permitsPerSeconds, readMicros());
    }
}
