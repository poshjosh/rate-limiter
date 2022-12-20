package com.looseboxes.ratelimiter.rates;

import com.looseboxes.ratelimiter.Rate;
import com.looseboxes.ratelimiter.SerializationTest;
import org.junit.jupiter.api.Test;

class RateSerializationTest extends SerializationTest<Rate> {

    @Test
    void testSerialization() {
        testSerialization(1, 3000);
        testSerialization(99, 13_000);
        testSerialization(1_000_000, 20);
    }

    void testSerialization(long amount, long duration) {
        super.testSerialization(Rate.of(amount, duration));
    }
}
