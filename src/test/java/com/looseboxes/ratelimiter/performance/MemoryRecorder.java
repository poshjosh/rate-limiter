package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.Util;

import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

public class MemoryRecorder implements ConsumptionRecorder<Long>{

    private final Map<Object, Long> records;

    public MemoryRecorder() {
        this.records = new HashMap<>();
    }

    @Override public Long recordCurrent(Object key) {
        return records.put(key, Util.availableMemory());
    }

    @Override public Long getConsumedSinceLastRecord(Object key) {
        return Util.usedMemory(records.get(key));
    }
}
