package com.looseboxes.ratelimiter.performance;

public interface ConsumptionRecorder<V> {
    V recordCurrent(Object key);
    V getConsumedSinceLastRecord(Object key);
}
