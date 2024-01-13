package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.model.Rate;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class RateToBandwidthConverterTest {

    RateToBandwidthConverter rateToBandwidthConverter = RateToBandwidthConverter.ofDefaults();

    @Test
    void convert() {
        final int permits = 1;
        Bandwidth bandwidth = rateToBandwidthConverter.convert(Rate.ofSeconds(permits));
        assertTrue(bandwidth.isAvailable(0, 0));
        bandwidth.reserveAndGetWaitLength(permits, 0);
        assertFalse(bandwidth.isAvailable(0, 0));
    }
}