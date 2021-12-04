package com.looseboxes.ratelimiter.performance;

import java.util.HashMap;
import java.util.Map;

public class TimeRecorder implements ConsumptionRecorder<Long>{

    private final Map<Object, Long> records;

    public TimeRecorder() {
        this.records = new HashMap<>();
    }

    @Override public Long recordCurrent(Object key) {
        return records.put(key, System.currentTimeMillis());
    }

    @Override public Long getConsumedSinceLastRecord(Object key) {
        return (System.currentTimeMillis() - records.get(key));
    }
}
