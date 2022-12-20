package com.looseboxes.ratelimiter.bandwidths;

import com.looseboxes.ratelimiter.util.SleepingTicker;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;

abstract class BandwidthTest {

    double permitsPerSeconds = 1;

    final SleepingTicker ticker = SleepingTicker.systemTicker();

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

    @Test
    void copy_shouldReturnCopy() {
        Bandwidth one = getBandwidth();
        Bandwidth two = one.copy();
        assertEquals(one, two);
    }

    @Test
    void copy_shouldUseTheCorrectRate() {
        Bandwidth bandwidth = getBandwidth();
        final long micros = readMicros();
        Bandwidth copy1 = bandwidth.copy(micros);
        Bandwidth copy2 = bandwidth.copy(bandwidth.getRate(), micros);
        assertEquals(copy1, copy2);
    }

    @Test
    void setRate_shouldReturnCopy() {
        // Has been known to fail with the following message:
        // expected: <SmoothBurstyBandwidth{storedPermits=3.0E-5,                maxPermits=5.0, stableIntervalMicros=200000.0, maxBurstSeconds=1.0, nextFreeTicketMicros=335}>
        //  but was: <SmoothBurstyBandwidth{storedPermits=3.0000000000000004E-5, maxPermits=5.0, stableIntervalMicros=200000.0, maxBurstSeconds=1.0, nextFreeTicketMicros=335}>
        Bandwidth bandwidth = getBandwidth();
        final int permitsPerSec = 5;
        final long nowMicros = readMicros();
        bandwidth.setRate(permitsPerSec, nowMicros);
        final Bandwidth copy = bandwidth.copy(permitsPerSec, nowMicros);
        assertEquals(bandwidth, copy);
    }

    @Test
    void copy_afterMultipleOperations_shouldReturnCopy() {
        Bandwidth bandwidth = getBandwidth();
        bandwidth.reserveNextAvailable((int)permitsPerSeconds, readMicros());
        bandwidth.reserveNextAvailable((int)permitsPerSeconds, readMicros());
        ticker.sleepMicrosUninterruptibly(1000);
        Bandwidth copy = bandwidth.copy();
        assertEquals(bandwidth, copy);
    }

    @ParameterizedTest
    @ValueSource(doubles = {0.1, 1, 100_000, Double.MAX_VALUE})
    void copy_givenPermitsPerSecond_shouldReturnCopyWithUpdates(double permitsPerSecond) {
        Bandwidth bandwidth = getBandwidth(permitsPerSecond);
        Bandwidth copy = bandwidth.copy();
        assertEquals(bandwidth, copy);
    }

    protected long readMicros() {
        return ticker.elapsed(TimeUnit.MICROSECONDS);
    }
}
