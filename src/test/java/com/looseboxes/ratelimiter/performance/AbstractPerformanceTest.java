package com.looseboxes.ratelimiter.performance;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

public class AbstractPerformanceTest {

    private final ConsumptionRecorder<Long> timeRecorder = new TimeRecorder();
    private final ConsumptionRecorder<Long> memoryRecorder = new MemoryRecorder();

    void recordCurrentTimeAndMemory() {
        recordCurrentTime();
        recordCurrentMemory();
    }

    void recordCurrentTime() {
        timeRecorder.recordCurrent(getTimeConsumptionKey());
    }

    void recordCurrentMemory() {
        memoryRecorder.recordCurrent(getMemoryConsumptionKey());
    }

    void assertTimeSinceLastRecordIsLessThan(long maxTime) {
        final long timeSpent = timeRecorder.getConsumedSinceLastRecord(getTimeConsumptionKey());
        System.out.printf("Spent time: %d.%d seconds\n", (timeSpent/1000), (timeSpent%1000));
        assertThat(timeSpent).isLessThanOrEqualTo(maxTime);
    }

    void assertMemorySinceLastRecordIsLessThan(long maxMemory) {
        final long memorySpent = memoryRecorder.getConsumedSinceLastRecord(getMemoryConsumptionKey());
        System.out.printf("Spent memory: %d.%d MB (%d bytes)\n",
                (memorySpent/1000000), (memorySpent%1000000), memorySpent);
        assertThat(memorySpent).isLessThanOrEqualTo(maxMemory);
    }

    private Object getTimeConsumptionKey() {
        return this.getClass().getName() + "-time";
    }

    private Object getMemoryConsumptionKey() {
        return this.getClass().getName() + "-memory";
    }
}
