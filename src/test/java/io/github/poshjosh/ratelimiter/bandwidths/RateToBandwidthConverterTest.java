package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.model.Rate;
import io.github.poshjosh.ratelimiter.util.Ticker;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class RateToBandwidthConverterTest {

    Ticker ticker = Ticker.ofDefaults();
    RateToBandwidthConverter rateToBandwidthConverter = RateToBandwidthConverter.ofDefaults();

    @Test
    void convert() {
        final int permits = 1;
        Bandwidth bandwidth = rateToBandwidthConverter.convert(Rate.ofSeconds(permits));

        assertTrue(bandwidth.isAvailable(ticker.elapsedMicros(), 0));
        bandwidth.reserveAndGetWaitLength(permits, ticker.elapsedMicros());
        assertFalse(bandwidth.isAvailable(ticker.elapsedMicros(), 0));
    }
}