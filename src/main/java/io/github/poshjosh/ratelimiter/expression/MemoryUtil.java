package io.github.poshjosh.ratelimiter.expression;

final class MemoryUtil {
    private static final Runtime runtime = Runtime.getRuntime();
    // Max heap VM can use e.g. Xmx setting
    private static final long maxMemory = runtime.maxMemory();
    private MemoryUtil() { }
    static long availableMemory() {
        return maxMemory - usedMemory(); // available memory i.e. Maximum heap size minus the bookmark amount used
    }
    static long usedMemory() {
        final long total = runtime.totalMemory(); // bookmark heap allocated to the VM process
        final long free = runtime.freeMemory(); // out of the bookmark heap, how much is free
        return total - free; // how much of the bookmark heap the VM is using
    }
    static long freeMemory() {
        return runtime.freeMemory();
    }
    static long maxMemory() {
        return maxMemory;
    }
    static long totalMemory() {
        return runtime.totalMemory();
    }
}
