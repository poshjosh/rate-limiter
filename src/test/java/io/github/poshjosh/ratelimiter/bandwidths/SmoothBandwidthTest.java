package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.util.Ticker;
import io.github.poshjosh.ratelimiter.util.Tickers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

abstract class SmoothBandwidthTest {

    double permitsPerSeconds = 1;

    final Ticker ticker = Tickers.ofDefaults();

    protected SmoothBandwidth getBandwidth() {
        return getBandwidth(permitsPerSeconds);
    }

    protected abstract SmoothBandwidth getBandwidth(double permitsPerSeconds);

    @ParameterizedTest
    @ValueSource(doubles = {0.1, 1.0, 100.001, 100_000, Long.MAX_VALUE})
    void getRateShouldReturnArgumentOfSetRate(double expected) {
        SmoothBandwidth bandwidth = getBandwidth();
        bandwidth.setPermitsPerSecond(expected, readMicros());
        assertEquals(expected, bandwidth.getPermitsPerSecond());
    }

    protected long readMicros() {
        return ticker.elapsedMicros();
    }
}
