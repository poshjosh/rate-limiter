package io.github.poshjosh.ratelimiter.bandwidths;

import org.junit.jupiter.api.Test;

class SmoothWarmingUpBandwithSerializationTest extends SerializationTest<SmoothWarmingUpBandwidth> {

    @Test
    void testSerialization() {
        testSerialization(1, 3000, 1_000);
        testSerialization(99, 1, 2);
        testSerialization(0.1, 20, 200);
    }

    void testSerialization(double permitsPerSecond, long nowMicros, long warmupPeriodSeconds) {
        super.testSerialization((SmoothWarmingUpBandwidth)
                Bandwidths.warmingUp(permitsPerSecond, nowMicros, warmupPeriodSeconds));
    }
}
