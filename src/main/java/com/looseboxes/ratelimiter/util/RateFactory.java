package com.looseboxes.ratelimiter.util;

import com.looseboxes.ratelimiter.annotation.RateComposition;

import java.util.List;

public interface RateFactory<K> {
    List<RateComposition<K>> getRates();
}
