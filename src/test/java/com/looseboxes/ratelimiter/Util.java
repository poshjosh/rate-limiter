package com.looseboxes.ratelimiter;

public class Util {
    public final static long availableMemory() {
        final Runtime runtime = Runtime.getRuntime();
        final long max = runtime.maxMemory(); // Max heap VM can use e.g. Xmx setting
        final long availableHeapMemory = max - _usedMemory(runtime); // available memory i.e. Maximum heap size minus the current amount used
        return availableHeapMemory;
    }

    public final static long usedMemory() {
        return _usedMemory(Runtime.getRuntime());
    }

    private static long _usedMemory(Runtime runtime) {
        final long total = runtime.totalMemory(); // current heap allocated to the VM process
        final long free = runtime.freeMemory(); // out of the current heap, how much is free
        final long used = total - free; // how much of the current heap the VM is using
        return used;
    }

    public final static long usedMemory(long bookmarkMemory) {
        return bookmarkMemory - availableMemory();
    }
}
