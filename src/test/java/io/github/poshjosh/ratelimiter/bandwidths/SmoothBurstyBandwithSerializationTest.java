package io.github.poshjosh.ratelimiter.bandwidths;

import org.junit.jupiter.api.Test;

class SmoothBurstyBandwithSerializationTest extends SerializationTest<SmoothBurstyBandwidth> {

    @Test
    void testSerialization() {
        testSerialization(1, 3000, 1);
        testSerialization(99, 1, 2.5);
        testSerialization(0.1, 20, 20);
    }

    void testSerialization(double permitsPerSecond, long nowMicros, double maxBurstsSecond) {
        super.testSerialization((SmoothBurstyBandwidth)
                Bandwidths.bursty(permitsPerSecond, nowMicros, maxBurstsSecond));
    }
}
