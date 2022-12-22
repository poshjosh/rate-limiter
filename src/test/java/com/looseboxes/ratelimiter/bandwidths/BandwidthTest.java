package com.looseboxes.ratelimiter.bandwidths;

import com.looseboxes.ratelimiter.util.SleepingTicker;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

abstract class BandwidthTest {

    double permitsPerSeconds = 1;

    final SleepingTicker ticker = SleepingTicker.zeroOffset();

    protected Bandwidth getBandwidth() {
        return getBandwidth(permitsPerSeconds);
    }

    protected abstract Bandwidth getBandwidth(double permitsPerSeconds);

    @ParameterizedTest
    @ValueSource(longs = {0, 1000, 100_000, Long.MAX_VALUE})
    void getRateShouldReturnArgumentOfSetRate(long nowMicros) {
        Bandwidth bandwidth = getBandwidth();
        double expected = 20;
        bandwidth.setRate(expected, nowMicros);
        assertEquals(expected, bandwidth.getRate());
    }

    protected long readMicros() {
        return ticker.elapsedMicros();
    }
}
