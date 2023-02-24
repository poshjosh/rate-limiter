package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.RateLimiter;
import io.github.poshjosh.ratelimiter.Operator;
import io.github.poshjosh.ratelimiter.Ticker;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.concurrent.TimeUnit;

import static java.util.concurrent.TimeUnit.*;

class BandwidthTest {

    @FunctionalInterface
    interface BandwidthFactory{
        Bandwidth createNew(long permits, long duration, TimeUnit timeUnit, long nowMicros);
    }

    /**
     * The ticker gathers events and presents them as strings. R0.6 means a delay of 0.6 seconds
     * caused by the (R)ateLimiter U1.0 means the (U)ser caused the ticker to sleep for a second.
     */
    static class FakeTicker implements Ticker {
        long instant = 0L;
        @Override
        public long elapsedNanos() {
            return instant;
        }
        void sleepMicros(long micros) {
            instant += MICROSECONDS.toNanos(micros);
        }
        @Override
        public void sleepMicrosWithoutInterruption(long sleepFor) {
            sleepMicros(sleepFor);
        }
    }

    private final FakeTicker ticker = new FakeTicker();

    @ParameterizedTest
    @ValueSource(doubles = {0.1, 0.5, 1, 3, 33, 101})
    void testBursty(double permitsPerSecond) {
        testBandwidth(burstyBandwidthFactory(), permitsPerSecond);
    }

    @ParameterizedTest
    @ValueSource(doubles = {0.1, 0.5, 1, 3, 33, 101})
    void testWarmingUp(double permitsPerSecond) {
        testBandwidth(warmingUpBandwidthFactory(), permitsPerSecond);
    }
    private void testBandwidth(BandwidthFactory bandwidthFactory, double permitsPerSecond) {
        long permitsPerDay = (long)(permitsPerSecond * TimeUnit.DAYS.toSeconds(1));
        final Bandwidth bandwidth = bandwidthFactory.createNew(permitsPerDay, 1, DAYS,  ticker.elapsedMicros());
        final RateLimiter limiter = createRateLimiter(Bandwidths.of(bandwidth), ticker);

        final double max = Math.max(permitsPerSecond, 1);
        int i = 0;
        for (; i < max; i++) {
            //System.out.println(i);
            if (i > 0) {
                ticker.sleepMicros(getStableIntervalMicros(bandwidth));
            }
            assertTrue("Unable to acquire permit: " + i, limiter.tryAcquire());
        }
        //System.out.println(i);
        assertFalse("Capable of acquiring permit: " + i, limiter.tryAcquire());
    }

    @Test
    void testAllOrNothing() {
        BandwidthFactory bandwidthFactory = Bandwidth::allOrNothing;
        // This fails because by the time we get to 2, we would have entered the 2nd millisecond
        //testAllOrNothing(bandwidthFactory, 2, 1, MILLISECONDS);
        testAllOrNothing(bandwidthFactory, 1, 10, SECONDS);
        testAllOrNothing(bandwidthFactory, 1, 2, SECONDS);
        testAllOrNothing(bandwidthFactory, 1, 1, SECONDS);
        testAllOrNothing(bandwidthFactory, 3, 1, SECONDS);
        testAllOrNothing(bandwidthFactory, 33, 1, SECONDS);
        // This fails because by the time we get to 101, we would have entered the 2nd second
        //testAllOrNothing(bandwidthFactory, 101, 1, SECONDS);
    }
    private void testAllOrNothing(BandwidthFactory bandwidthFactory, long permits, long duration, TimeUnit unit) {
        final Ticker ticker = Ticker.ofDefaults();
        final Bandwidth bandwidth = bandwidthFactory.createNew(permits, duration, unit, ticker.elapsedMicros());
        final RateLimiter limiter = RateLimiter.of(Bandwidths.of(bandwidth), ticker);
        final double max = permits;
        int i = 0;
        for (; i < max; i++) {
            //System.out.println(i);
            assertTrue("Unable to acquire permit: " + i, limiter.tryAcquire());
        }
        //System.out.println(i);
        assertFalse("Capable of acquiring permit: " + i, limiter.tryAcquire());
    }

    @ParameterizedTest
    @EnumSource(Operator.class)
    void testBurstyBandwidths(Operator operator) {
        testBandwidths(burstyBandwidthFactory(), operator, true);
    }

    private BandwidthFactory burstyBandwidthFactory() {
        return (permits, duration, unit, now) ->
                Bandwidth.bursty(toPermitsPerSecond(permits, duration, unit), now);
    }

    @ParameterizedTest
    @EnumSource(Operator.class)
    void testWarmingUpBandwidths(Operator operator) {
        testBandwidths(warmingUpBandwidthFactory(), operator, true);
    }

    private BandwidthFactory warmingUpBandwidthFactory() {
        // Note the behaviour differs for different values of: warmupPeriod and coldFactor.
        return (permits, duration, unit, now) ->
                Bandwidth.warmingUp(toPermitsPerSecond(permits, duration, unit), now, 1, SECONDS, 1);
    }

    private double toPermitsPerSecond(final long amount, final long duration, final TimeUnit timeUnit) {
        // We use the highest precision
        final long nanosDuration = timeUnit.toNanos(duration);
        final double perNanos = (double)amount / nanosDuration;
        // Won't work because it will return zero if the result is a fraction
        //SECONDS.convert((long)perNanos, NANOSECONDS);
        return perNanos * TimeUnit.SECONDS.toNanos(1L);
    }

    @ParameterizedTest
    @EnumSource(Operator.class)
    void testAllOrNothingBandwidths(Operator operator) {
        testBandwidths(Bandwidth::allOrNothing, operator, false);
    }

    private void testBandwidths(BandwidthFactory bandwidthFactory, Operator operator, boolean useInterval) {
        if (Operator.NONE.equals(operator)) {
            return;
        }
        final int min = 1;
        final int max = 3;
        final long nowMicros = ticker.elapsedMicros();

        Bandwidth a = bandwidthFactory.createNew(min, 1, SECONDS, nowMicros);
        Bandwidth b = bandwidthFactory.createNew(max, 1, SECONDS, nowMicros);

        Bandwidth bandwidth = Bandwidths.of(operator, a, b);

        RateLimiter limiter = createRateLimiter(bandwidth, ticker);

        int i = 0;
        final int limit = Operator.AND.equals(operator) ? max : min;
        for(; i < limit; i++) {
            //System.out.println(i);
            if (useInterval && i > 0) {
                ticker.sleepMicros(getStableIntervalMicros(bandwidth));
            }
            assertTrue("Unable to acquire permit: " + i, limiter.tryAcquire());
        }
        //System.out.println(i);
        assertFalse("Capable of acquiring permit: " + i, limiter.tryAcquire());
    }

    private RateLimiter createRateLimiter(Bandwidth bandwidth, Ticker ticker) {
        return RateLimiter.of(bandwidth, ticker);
    }

    private long getStableIntervalMicros(Bandwidth bandwidth) {
        final double permitsPerSecond = bandwidth.getPermitsPerSecond();
        return (long)(SECONDS.toMicros(1L) / permitsPerSecond);
    }

    private long getStableIntervalMicros(Bandwidths bandwidths) {
        final double permitsPerSecond = bandwidths.getPermitsPerSecond();
        return (long)(SECONDS.toMicros(1L) / permitsPerSecond);
    }

    private static void assertTrue(String message, boolean expression) {
        if (!expression) {
            throw new AssertionError(message);
        }
    }

    private static void assertFalse(String message, boolean expression) {
        if (expression) {
            throw new AssertionError(message);
        }
    }
}
