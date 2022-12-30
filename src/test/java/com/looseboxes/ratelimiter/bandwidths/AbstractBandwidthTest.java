package com.looseboxes.ratelimiter.bandwidths;

import com.looseboxes.ratelimiter.util.SleepingTicker;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

public abstract class AbstractBandwidthTest {

    private final SleepingTicker ticker = SleepingTicker.zeroOffset();

    protected abstract Bandwidth getBandwidth(double permitsPerSeconds, long nowMicros);

    @Test
    void with_givenSameMicros_shouldReturnExactCopy() throws InterruptedException{
        final long nowMicros = readMicros();
        Bandwidth expected = getBandwidth(5, nowMicros);
        Thread.sleep(77);
        assertEquals(expected, expected.with(nowMicros));
    }

    @Test
    void with_afterModifications_shouldNotReturnExactCopy() throws InterruptedException{
        Bandwidth expected = getBandwidth(5, readMicros());
        expected.queryEarliestAvailable(readMicros());
        expected.reserveEarliestAvailable(1, readMicros());
        Thread.sleep(77);
        expected.queryEarliestAvailable(readMicros());
        assertNotEquals(expected, expected.with(readMicros()));
    }

    protected Bandwidth getBandwidth(double permitsPerSeconds) {
        return getBandwidth(permitsPerSeconds, readMicros());
    }

    protected long readMicros() {
        return ticker.elapsedMicros();
    }
}
