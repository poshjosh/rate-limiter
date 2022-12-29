package com.looseboxes.ratelimiter.bandwidths;

import org.junit.jupiter.api.Test;

class BandwithsSerializationTest extends SerializationTest<Bandwidths> {

    @Test
    void testSerialization() {
        testSerialization(1, 3000, 1);
        testSerialization(99, 1, 3);
        testSerialization(0.1, 20, 20);
    }

    void testSerialization(double permitsPerSecond, long nowMicros, long warmUpPeriodSeconds) {
        Bandwidth a = Bandwidth.bursty(permitsPerSecond, nowMicros);
        Bandwidth b = Bandwidth.warmingUp(permitsPerSecond, nowMicros, warmUpPeriodSeconds);
        super.testSerialization(Bandwidths.and(a, b));
        super.testSerialization(Bandwidths.or(a, b));
    }
}
