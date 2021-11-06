package com.looseboxes.ratelimiter.util;

import com.looseboxes.ratelimiter.rates.Rate;

import java.util.Map;

public interface RateFactory<K> {
    Map<K, Rate[]> getRates();
}
