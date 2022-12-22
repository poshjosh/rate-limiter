package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.util.Rates;

public interface BandwidthLimiterProvider<K> {
    Bandwidths createBandwidths(K key, Rates limits);
    BandwidthLimiter getOrCreateBandwidthLimiter(K key, Bandwidths bandwidths);
}
