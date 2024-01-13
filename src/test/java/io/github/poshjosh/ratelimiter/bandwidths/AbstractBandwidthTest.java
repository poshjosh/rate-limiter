package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.util.Ticker;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

public abstract class AbstractBandwidthTest {

    private final Ticker ticker = Ticker.ofDefaults();

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

    @ParameterizedTest
    @ValueSource(longs = {0, 9, Long.MAX_VALUE})
    void testGetPermitsPerSecond(long permitsPerSecond) {
        Bandwidth bandwidth = getBandwidth(permitsPerSecond, readMicros());
        assertEquals(permitsPerSecond, bandwidth.getPermitsPerSecond());
    }

    protected long readMicros() {
        return ticker.elapsedMicros();
    }
}
