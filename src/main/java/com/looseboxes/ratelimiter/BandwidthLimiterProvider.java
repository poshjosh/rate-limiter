package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.util.SleepingTicker;

public interface BandwidthLimiterProvider<K> {

    SleepingTicker getTicker(K key);

    BandwidthLimiter getBandwidthLimiter(K key, Bandwidths bandwidths);
}
