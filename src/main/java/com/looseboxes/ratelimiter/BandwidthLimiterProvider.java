package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.util.Operator;
import com.looseboxes.ratelimiter.util.SleepingTicker;

public interface BandwidthLimiterProvider<K> {

    SleepingTicker getTicker(K key);

    BandwidthLimiter getBandwidthLimiter(K key, Bandwidth [] bandwidth, Operator operator);
}
