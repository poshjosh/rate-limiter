package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.Util;

import java.util.Objects;

public final class Usage {

    public static Usage bookmark() {
        return of(System.currentTimeMillis(), Util.availableMemory());
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

    public long getDuration() {
        return duration;
    }
    public long getMemory() {
        return memory;
    }

    public Usage usage() {
        return new Usage(System.currentTimeMillis() - duration, Util.usedMemory(memory));
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
        return super.toString() + "{duration=" + duration + ", memory=" + memory + '}';
    }
}
