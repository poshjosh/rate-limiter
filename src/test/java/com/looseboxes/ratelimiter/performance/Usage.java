package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.MemoryUtil;

import java.util.Objects;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

public final class Usage {

    public static Usage bookmark() {
        return of(System.currentTimeMillis(), MemoryUtil.availableMemory());
    }
    public static Usage of(long duration, long memory) {
        return new Usage(duration, memory);
    }

    private final long duration;
    private final long memory;

    public Usage(long duration, long memory) {
        this.duration = duration;
        this.memory = memory;
    }

    public void assertUsageLessThan(Usage limit) {
        Usage usage = usage();
        System.out.printf("Spent %s", DurationText.of(usage.getDuration()));
        assertThat(usage.getDuration()).isLessThanOrEqualTo(limit.getDuration());
        System.out.printf(", %s", ByteText.of(usage.getMemory()));
        assertThat(usage.getMemory()).isLessThanOrEqualTo(limit.getMemory());
    }

    public long getDuration() {
        return duration;
    }
    public long getMemory() {
        return memory;
    }

    public Usage usage() {
        return new Usage(System.currentTimeMillis() - duration, MemoryUtil.usedMemory(memory));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        Usage that = (Usage) o;
        return duration == that.duration && memory == that.memory;
    }

    @Override
    public int hashCode() {
        return Objects.hash(duration, memory);
    }

    @Override
    public String toString() {
        return "Usage{duration=" + duration + ", memory=" + memory + '}';
    }
}
