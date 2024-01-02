package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.model.Rate;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class RateToBandwidthConverterTest {

    RateToBandwidthConverter uut = RateToBandwidthConverter.ofDefaults();

    @Test
    void convert() {
        final int permits = 1;
        Bandwidth bandwidth = uut.convert(Rate.ofSeconds(permits), 0);
        assertTrue(bandwidth.canAcquire(0, 0));
        bandwidth.reserveAndGetWaitLength(permits, 0);
        assertFalse(bandwidth.canAcquire(0, 0));
    }
}