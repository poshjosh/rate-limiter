package com.looseboxes.ratelimiter;

import static java.lang.reflect.Modifier.isStatic;
import static java.util.concurrent.TimeUnit.*;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.Method;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.TimeUnit;

import com.looseboxes.ratelimiter.bandwidths.*;
import com.looseboxes.ratelimiter.util.Operator;
import com.looseboxes.ratelimiter.util.SleepingTicker;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mockito;

/**
 * Tests for BandwidthLimiter.
 */
class BandwidthLimiterTest {
    private static final double EPSILON = 1e-8;

    private final FakeTicker ticker = new FakeTicker();

    @ParameterizedTest
    @ValueSource(doubles = {0.1, 0.5, 1, 3, 33, 101})
    void testBursty(double permitsPerSecond) {
        testBandwidth(BandwidthFactory.bursty(), permitsPerSecond);
    }

    @ParameterizedTest
    @ValueSource(doubles = {0.1, 0.5, 1, 3, 33, 101})
    void testWarmingUp(double permitsPerSecond) {
        testBandwidth(BandwidthFactory.warmingUp(1, SECONDS, 1), permitsPerSecond);
    }
    private void testBandwidth(BandwidthFactory bandwidthFactory, double permitsPerSecond) {
        long permitsPerDay = (long)(permitsPerSecond * TimeUnit.DAYS.toSeconds(1));
        final Bandwidth bandwidth = bandwidthFactory.createNew(permitsPerDay, 1, DAYS,  ticker.elapsedMicros());
        final BandwidthLimiter limiter = createBandwidthLimiter(Bandwidths.of(bandwidth), ticker);

        final double max = Math.max(permitsPerSecond, 1);
        int i = 0;
        for (; i < max; i++) {
            //System.out.println(i);
            if (i > 0) {
                ticker.sleepMicros("U", getStableIntervalMicros(bandwidth));
            }
            assertTrue("Unable to acquire permit: " + i, limiter.tryAcquire());
        }
        //System.out.println(i);
        assertFalse("Capable of acquiring permit: " + i, limiter.tryAcquire());
    }

    @Test
    void testAllOrNothing() {
        BandwidthFactory bandwidthFactory = BandwidthFactory.allOrNothing();
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
        final SleepingTicker ticker = SleepingTicker.zeroOffset();
        final Bandwidth bandwidth = bandwidthFactory.createNew(permits, duration, unit, ticker.elapsedMicros());
        final BandwidthLimiter limiter = BandwidthLimiter.of(Bandwidths.of(bandwidth), ticker);
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
        testBandwidths(BandwidthFactory.bursty(), operator, true);
    }

    @ParameterizedTest
    @EnumSource(Operator.class)
    void testWarmingUpBandwidths(Operator operator) {
        // Note the behaviour differs for different values of: warmupPeriod and coldFactor.
        testBandwidths(BandwidthFactory.warmingUp(1, SECONDS, 1), operator, true);
    }

    @ParameterizedTest
    @EnumSource(Operator.class)
    void testAllOrNothingBandwidths(Operator operator) {
        testBandwidths(BandwidthFactory.allOrNothing(), operator, false);
    }

    private void testBandwidths(BandwidthFactory bandwidthFactory, Operator operator, boolean useInterval) {
        final int min = 1;
        final int max = 3;
        final long nowMicros = ticker.elapsedMicros();

        Bandwidth a = bandwidthFactory.createNew(min, Duration.ofSeconds(1), nowMicros);
        Bandwidth b = bandwidthFactory.createNew(max, Duration.ofSeconds(1), nowMicros);

        Bandwidths bandwidths = Bandwidths.of(operator, a, b);

        BandwidthLimiter limiter = createBandwidthLimiter(bandwidths, ticker);

        int i = 0;
        final int limit = Operator.AND.equals(operator) ? max : min;
        for(; i < limit; i++) {
            //System.out.println(i);
            if (useInterval && i > 0) {
                ticker.sleepMicros("U", getStableIntervalMicros(bandwidths));
            }
            assertTrue("Unable to acquire permit: " + i, limiter.tryAcquire());
        }
        //System.out.println(i);
        assertFalse("Capable of acquiring permit: " + i, limiter.tryAcquire());
    }

    private BandwidthLimiter createBandwidthLimiter(Bandwidths bandwidths, SleepingTicker ticker) {
        return BandwidthLimiter.of(bandwidths, ticker);
    }

    private long getStableIntervalMicros(Bandwidth bandwidth) {
        final double permitsPerSecond = bandwidth.getRate();
        return (long)(SECONDS.toMicros(1L) / permitsPerSecond);
    }

    private long getStableIntervalMicros(Bandwidths bandwidths) {
        final double permitsPerSecond = bandwidths.getRate();
        return (long)(SECONDS.toMicros(1L) / permitsPerSecond);
    }

    @Test
    public void testSimple() {
        BandwidthLimiter limiter = create(5.0, ticker);
        limiter.acquire(); // R0.00, since it's the first request
        limiter.acquire(); // R0.20
        limiter.acquire(); // R0.20
        assertEvents("R0.00", "R0.20", "R0.20");
    }

    @Test
    public void testImmediateTryAcquire() {
        BandwidthLimiter limiter = create(1);
        assertTrue("Unable to acquire initial permit", limiter.tryAcquire());
        assertFalse("Capable of acquiring secondary permit", limiter.tryAcquire());
    }

    @Test
    public void testDoubleMinValueCanAcquireExactlyOnce() {
        BandwidthLimiter limiter = create(Double.MIN_VALUE, ticker);
        assertTrue("Unable to acquire initial permit", limiter.tryAcquire());
        assertFalse("Capable of acquiring an additional permit", limiter.tryAcquire());
        ticker.sleepMillis(Integer.MAX_VALUE);
        assertFalse("Capable of acquiring an additional permit after sleeping", limiter.tryAcquire());
    }

    @Test
    public void testSimpleRateUpdate() {
        BandwidthLimiter limiter = create(5.0, 5, SECONDS);
        assertEquals(5.0, limiter.getPermitsPerSecond());
        limiter = setRate(limiter, 10.0);
        assertEquals(10.0, limiter.getPermitsPerSecond());
    }

    @Test
    public void testWithParameterValidation() {
        BandwidthLimiter limiter = create(5.0, 5, SECONDS);
        assertThrowsIllegalArgumentException(() -> setRate(limiter, 0.0));
        assertThrowsIllegalArgumentException(() -> setRate(limiter, -10.0));
    }

    @Test
    public void testAcquireParameterValidation() {
        BandwidthLimiter limiter = create(999);
        assertThrowsIllegalArgumentException(() -> limiter.acquire(0));
        assertThrowsIllegalArgumentException(() -> limiter.acquire(-1));
        assertThrowsIllegalArgumentException(() -> limiter.tryAcquire(0));
        assertThrowsIllegalArgumentException(() -> limiter.tryAcquire(-1));
        assertThrowsIllegalArgumentException(() -> limiter.tryAcquire(0, 1, SECONDS));
        assertThrowsIllegalArgumentException(() -> limiter.tryAcquire(-1, 1, SECONDS));
    }

    @Test
    public void testSimpleWithWait() {
        BandwidthLimiter limiter = create(5.0, ticker);
        limiter.acquire(); // R0.00
        ticker.sleepMillis(200); // U0.20, we are ready for the next request...
        limiter.acquire(); // R0.00, ...which is granted immediately
        limiter.acquire(); // R0.20
        assertEvents("R0.00", "U0.20", "R0.00", "R0.20");
    }

    @Test
    public void testSimpleAcquireReturnValues() {
        BandwidthLimiter limiter = create(5.0, ticker);
        assertEquals(0.0, limiter.acquire(), EPSILON); // R0.00
        ticker.sleepMillis(200); // U0.20, we are ready for the next request...
        assertEquals(0.0, limiter.acquire(), EPSILON); // R0.00, ...which is granted immediately
        assertEquals(0.2, limiter.acquire(), EPSILON); // R0.20
        assertEvents("R0.00", "U0.20", "R0.00", "R0.20");
    }

    @Test
    public void testSimpleAcquireEarliestAvailableIsInPast() {
        BandwidthLimiter limiter = create(5.0, ticker);
        assertEquals(0.0, limiter.acquire(), EPSILON);
        ticker.sleepMillis(400);
        assertEquals(0.0, limiter.acquire(), EPSILON);
        assertEquals(0.0, limiter.acquire(), EPSILON);
        assertEquals(0.2, limiter.acquire(), EPSILON);
    }

    @Test
    public void testOneSecondBurst() {
        BandwidthLimiter limiter = create(5.0, ticker);
        ticker.sleepMillis(1000); // max capacity reached
        ticker.sleepMillis(1000); // this makes no difference
        limiter.acquire(1); // R0.00, since it's the first request

        limiter.acquire(1); // R0.00, from capacity
        limiter.acquire(3); // R0.00, from capacity
        limiter.acquire(1); // R0.00, concluding a burst of 5 permits

        limiter.acquire(); // R0.20, capacity exhausted
        assertEvents(
                "U1.00", "U1.00", "R0.00", "R0.00", "R0.00", "R0.00", // first request and burst
                "R0.20");
    }

    @Test
    public void testCreateWarmupParameterValidation() {
        BandwidthLimiter unused;
        unused = create(1.0, 1, NANOSECONDS);
        unused = create(1.0, 0, NANOSECONDS);

        assertThrowsIllegalArgumentException(() -> create(0.0, 1, NANOSECONDS));
        assertThrowsIllegalArgumentException(() -> create(-10.0, -1, NANOSECONDS));
        assertThrowsIllegalArgumentException(() -> create(1.0, -1, NANOSECONDS));
    }

    private static void assertThrowsIllegalArgumentException(Executable executable) {
        assertThrows(IllegalArgumentException.class, executable);
    }

    //@AndroidIncompatible // difference in String.format rounding?
    @Test
    public void testWarmUp() {
        BandwidthLimiter limiter = create(2.0, 4000, MILLISECONDS, 3.0, ticker);
        for (int i = 0; i < 8; i++) {
            limiter.acquire(); // #1
        }
        ticker.sleepMillis(500); // #2: to repay for the last acquire
        ticker.sleepMillis(4000); // #3: becomes cold again
        for (int i = 0; i < 8; i++) {
            limiter.acquire(); // // #4
        }
        ticker.sleepMillis(500); // #5: to repay for the last acquire
        ticker.sleepMillis(2000); // #6: didn't get cold! It would take another 2 seconds to go cold
        for (int i = 0; i < 8; i++) {
            limiter.acquire(); // #7
        }
        assertEvents(
                "R0.00, R1.38, R1.13, R0.88, R0.63, R0.50, R0.50, R0.50", // #1
                "U0.50", // #2
                "U4.00", // #3
                "R0.00, R1.38, R1.13, R0.88, R0.63, R0.50, R0.50, R0.50", // #4
                "U0.50", // #5
                "U2.00", // #6
                "R0.00, R0.50, R0.50, R0.50, R0.50, R0.50, R0.50, R0.50"); // #7
    }

    @Test
    public void testWarmUpWithColdFactor() {
        BandwidthLimiter limiter = create(5.0, 4000, MILLISECONDS, 10.0, ticker);
        for (int i = 0; i < 8; i++) {
            limiter.acquire(); // #1
        }
        ticker.sleepMillis(200); // #2: to repay for the last acquire
        ticker.sleepMillis(4000); // #3: becomes cold again
        for (int i = 0; i < 8; i++) {
            limiter.acquire(); // // #4
        }
        ticker.sleepMillis(200); // #5: to repay for the last acquire
        ticker.sleepMillis(1000); // #6: still warm! It would take another 3 seconds to go cold
        for (int i = 0; i < 8; i++) {
            limiter.acquire(); // #7
        }
        assertEvents(
                "R0.00, R1.75, R1.26, R0.76, R0.30, R0.20, R0.20, R0.20", // #1
                "U0.20", // #2
                "U4.00", // #3
                "R0.00, R1.75, R1.26, R0.76, R0.30, R0.20, R0.20, R0.20", // #4
                "U0.20", // #5
                "U1.00", // #6
                "R0.00, R0.20, R0.20, R0.20, R0.20, R0.20, R0.20, R0.20"); // #7
    }

    @Test
    public void testWarmUpWithColdFactor1() {
        BandwidthLimiter limiter = create(5.0, 4000, MILLISECONDS, 1.0, ticker);
        for (int i = 0; i < 8; i++) {
            limiter.acquire(); // #1
        }
        ticker.sleepMillis(340); // #2
        for (int i = 0; i < 8; i++) {
            limiter.acquire(); // #3
        }
        assertEvents(
                "R0.00, R0.20, R0.20, R0.20, R0.20, R0.20, R0.20, R0.20", // #1
                "U0.34", // #2
                "R0.00, R0.20, R0.20, R0.20, R0.20, R0.20, R0.20, R0.20"); // #3
    }

    //@AndroidIncompatible // difference in String.format rounding?
    @Test
    public void testWarmUpAndUpdate() {
        BandwidthLimiter limiter = create(2.0, 4000, MILLISECONDS, 3.0, ticker);
        for (int i = 0; i < 8; i++) {
            limiter.acquire(); // // #1
        }
        ticker.sleepMillis(4500); // #2: back to cold state (warmup period + repay last acquire)
        for (int i = 0; i < 3; i++) { // only three steps, we're somewhere in the warmup period
            limiter.acquire(); // #3
        }

        limiter = setRate(limiter, 4.0);
        limiter.acquire(); // #4, we repay the debt of the last acquire (imposed by the old rate)
        for (int i = 0; i < 4; i++) {
            limiter.acquire(); // #5
        }
        ticker.sleepMillis(4250); // #6, back to cold state (warmup period + repay last acquire)
        for (int i = 0; i < 11; i++) {
            limiter.acquire(); // #7, showing off the warmup starting from totally cold
        }

        // make sure the areas (times) remain the same, while permits are different
        assertEvents(
                "R0.00, R1.38, R1.13, R0.88, R0.63, R0.50, R0.50, R0.50", // #1
                "U4.50", // #2
                "R0.00, R1.38, R1.13", // #3, after that the rate changes
                "R0.88", // #4, this is what the throttling would be with the old rate
                "R0.34, R0.28, R0.25, R0.25", // #5
                "U4.25", // #6
                "R0.00, R0.72, R0.66, R0.59, R0.53, R0.47, R0.41", // #7
                "R0.34, R0.28, R0.25, R0.25"); // #7 (cont.), note, this matches #5
    }

    @Test
    public void testWarmUpAndUpdateWithColdFactor() {
        BandwidthLimiter limiter = create(5.0, 4000, MILLISECONDS, 10.0, ticker);
        for (int i = 0; i < 8; i++) {
            limiter.acquire(); // #1
        }
        ticker.sleepMillis(4200); // #2: back to cold state (warmup period + repay last acquire)
        for (int i = 0; i < 3; i++) { // only three steps, we're somewhere in the warmup period
            limiter.acquire(); // #3
        }

        limiter = setRate(limiter, 10.0); // double the rate!
        limiter.acquire(); // #4, we repay the debt of the last acquire (imposed by the old rate)
        for (int i = 0; i < 4; i++) {
            limiter.acquire(); // #5
        }
        ticker.sleepMillis(4100); // #6, back to cold state (warmup period + repay last acquire)
        for (int i = 0; i < 11; i++) {
            limiter.acquire(); // #7, showing off the warmup starting from totally cold
        }

        // make sure the areas (times) remain the same, while permits are different
        assertEvents(
                "R0.00, R1.75, R1.26, R0.76, R0.30, R0.20, R0.20, R0.20", // #1
                "U4.20", // #2
                "R0.00, R1.75, R1.26", // #3, after that the rate changes
                "R0.76", // #4, this is what the throttling would be with the old rate
                "R0.20, R0.10, R0.10, R0.10", // #5
                "U4.10", // #6
                "R0.00, R0.94, R0.81, R0.69, R0.57, R0.44, R0.32", // #7
                "R0.20, R0.10, R0.10, R0.10"); // #7 (cont.), note, this matches #5
    }

    @Test
    public void testBurstyAndUpdate() {
        BandwidthLimiter limiter = create(1.0, ticker);
        limiter.acquire(1); // no wait
        limiter.acquire(1); // R1.00, to repay previous

        limiter = setRate(limiter, 2.0); // setPermitsPerSecond the rate!

        limiter.acquire(1); // R1.00, to repay previous (the previous was under the old rate!)
        limiter.acquire(2); // R0.50, to repay previous (now the rate takes effect)
        limiter.acquire(4); // R1.00, to repay previous
        limiter.acquire(1); // R2.00, to repay previous
        assertEvents("R0.00", "R1.00", "R1.00", "R0.50", "R1.00", "R2.00");
    }

    @Test
    public void testTryAcquire_noWaitAllowed() {
        BandwidthLimiter limiter = create(5.0, ticker);
        assertTrue(limiter.tryAcquire(0, SECONDS));
        assertFalse(limiter.tryAcquire(0, SECONDS));
        assertFalse(limiter.tryAcquire(0, SECONDS));
        ticker.sleepMillis(100);
        assertFalse(limiter.tryAcquire(0, SECONDS));
    }

    @Test
    public void testTryAcquire_someWaitAllowed() {
        BandwidthLimiter limiter = create(5.0, ticker);
        assertTrue(limiter.tryAcquire(0, SECONDS));
        assertTrue(limiter.tryAcquire(200, MILLISECONDS));
        assertFalse(limiter.tryAcquire(100, MILLISECONDS));
        ticker.sleepMillis(100);
        assertTrue(limiter.tryAcquire(100, MILLISECONDS));
    }

    @Test
    public void testTryAcquire_overflow() {
        BandwidthLimiter limiter = create(5.0, ticker);
        assertTrue(limiter.tryAcquire(0, MICROSECONDS));
        ticker.sleepMillis(100);
        assertTrue(limiter.tryAcquire(Long.MAX_VALUE, MICROSECONDS));
    }

    @Test
    public void testTryAcquire_negative() {
        BandwidthLimiter limiter = create(5.0, ticker);
        assertTrue(limiter.tryAcquire(5, 0, SECONDS));
        ticker.sleepMillis(900);
        assertFalse(limiter.tryAcquire(1, Long.MIN_VALUE, SECONDS));
        ticker.sleepMillis(100);
        assertTrue(limiter.tryAcquire(1, -1, SECONDS));
    }

    @Test
    public void testSimpleWeights() {
        BandwidthLimiter limiter = create(1.0, ticker);
        limiter.acquire(1); // no wait
        limiter.acquire(1); // R1.00, to repay previous
        limiter.acquire(2); // R1.00, to repay previous
        limiter.acquire(4); // R2.00, to repay previous
        limiter.acquire(8); // R4.00, to repay previous
        limiter.acquire(1); // R8.00, to repay previous
        assertEvents("R0.00", "R1.00", "R1.00", "R2.00", "R4.00", "R8.00");
    }

    @Test
    public void testInfinity_Bursty() {
        BandwidthLimiter limiter = create(Double.POSITIVE_INFINITY, ticker);
        limiter.acquire(Integer.MAX_VALUE / 4);
        limiter.acquire(Integer.MAX_VALUE / 2);
        limiter.acquire(Integer.MAX_VALUE);
        assertEvents("R0.00", "R0.00", "R0.00"); // no wait, infinite rate!

        limiter = setRate(limiter, 2.0);
        limiter.acquire();
        limiter.acquire();
        limiter.acquire();
        limiter.acquire();
        limiter.acquire();
        assertEvents(
                "R0.00", // First comes the saved-up burst, which defaults to a 1-second burst (2 requests).
                "R0.00", "R0.00", // Now comes the free request.
                "R0.50", // Now it's 0.5 seconds per request.
                "R0.50");

        limiter = setRate(limiter, Double.POSITIVE_INFINITY);
        limiter.acquire();
        limiter.acquire();
        limiter.acquire();
        assertEvents("R0.50", "R0.00", "R0.00"); // we repay the last request (.5sec), then back to +oo
    }

    /** https://code.google.com/p/guava-libraries/issues/detail?id=1791 */
    @Test
    public void testInfinity_BustyTimeElapsed() {
        BandwidthLimiter limiter = create(Double.POSITIVE_INFINITY, ticker);
        ticker.instant += 1000000;
        limiter = setRate(limiter, 2.0);
        for (int i = 0; i < 5; i++) {
            limiter.acquire();
        }
        assertEvents(
                "R0.00", // First comes the saved-up burst, which defaults to a 1-second burst (2 requests).
                "R0.00", "R0.00", // Now comes the free request.
                "R0.50", // Now it's 0.5 seconds per request.
                "R0.50");
    }

    @Test
    public void testInfinity_WarmUp() {
        BandwidthLimiter limiter = create(Double.POSITIVE_INFINITY, 10, SECONDS, 3.0, ticker);
        limiter.acquire(Integer.MAX_VALUE / 4);
        limiter.acquire(Integer.MAX_VALUE / 2);
        limiter.acquire(Integer.MAX_VALUE);
        assertEvents("R0.00", "R0.00", "R0.00");

        limiter = setRate(limiter, 1.0);
        limiter.acquire();
        limiter.acquire();
        limiter.acquire();
        assertEvents("R0.00", "R1.00", "R1.00");

        limiter = setRate(limiter, Double.POSITIVE_INFINITY);
        limiter.acquire();
        limiter.acquire();
        limiter.acquire();
        assertEvents("R1.00", "R0.00", "R0.00");
    }

    @Test
    public void testInfinity_WarmUpTimeElapsed() {
        BandwidthLimiter limiter = create(Double.POSITIVE_INFINITY, 10, SECONDS, 3.0, ticker);
        ticker.instant += 1000000;
        limiter = setRate(limiter, 1.0);
        for (int i = 0; i < 5; i++) {
            limiter.acquire();
        }
        assertEvents("R0.00", "R1.00", "R1.00", "R1.00", "R1.00");
    }

    /**
     * Make sure that bursts can never go above 1-second-worth-of-work for the current rate, even when
     * we change the rate.
     */
    @Test
    public void testWeNeverGetABurstMoreThanOneSec() {
        BandwidthLimiter limiter = create(1.0, ticker);
        int[] rates = {1000, 1, 10, 1000000, 10, 1};
        for (int rate : rates) {
            int oneSecWorthOfWork = rate;
            ticker.sleepMillis(rate * 1000);
            limiter = setRate(limiter, rate);
            long burst = measureTotalTimeMillis(limiter, oneSecWorthOfWork, new Random());
            // we allow one second worth of work to go in a burst (i.e. take less than a second)
            assertTrue("Expected value <= 1000, found: " + burst, burst <= 1000);
            long afterBurst = measureTotalTimeMillis(limiter, oneSecWorthOfWork, new Random());
            // but work beyond that must take at least one second
            assertTrue("Expected value >= 1000, found: " + afterBurst, afterBurst >= 1000);
        }
    }
    
    private BandwidthLimiter setRate(BandwidthLimiter limiter, double permitsPerSecond) {
        ((DefaultBandwidthLimiter)limiter).setRate(permitsPerSecond);
        return limiter;
    }

    /**
     * This neat test shows that no matter what weights we use in our requests, if we push X amount of
     * permits in a cool state, where X = rate * timeToCoolDown, and we have specified a
     * timeToWarmUp() period, it will cost as the prescribed amount of time. E.g., calling
     * [acquire(5), acquire(1)] takes exactly the same time as [acquire(2), acquire(3), acquire(1)].
     */
    @Test
    public void testTimeToWarmUpIsHonouredEvenWithWeights() {
        Random random = new Random();
        int warmupPermits = 10;
        double[] coldFactorsToTest = {2.0, 3.0, 10.0};
        double[] qpsToTest = {4.0, 2.0, 1.0, 0.5, 0.1};
        for (int trial = 0; trial < 100; trial++) {
            for (double coldFactor : coldFactorsToTest) {
                for (double qps : qpsToTest) {
                    // If warmupPermits = maxPermits - thresholdPermits then
                    // warmupPeriod = (1 + coldFactor) * warmupPermits * stableInterval / 2
                    long warmupMillis = (long) ((1 + coldFactor) * warmupPermits / (2.0 * qps) * 1000.0);
                    BandwidthLimiter limiter = create(qps, warmupMillis, MILLISECONDS, coldFactor, ticker);
                    assertEquals(warmupMillis, measureTotalTimeMillis(limiter, warmupPermits, random));
                }
            }
        }
    }

    @Test
    public void testNulls() {
        // @TODO
    }

    @Test
    public void testVerySmallDoubleValues() {
        BandwidthLimiter limiter = create(Double.MIN_VALUE, ticker);
        assertTrue("Should acquire initial permit", limiter.tryAcquire());
        assertFalse("Should not acquire additional permit", limiter.tryAcquire());
        ticker.sleepMillis(5000);
        assertFalse(
                "Should not acquire additional permit even after sleeping", limiter.tryAcquire());
    }

    private static BandwidthLimiter create(double permitsPerSecond) {
        return create(permitsPerSecond, SleepingTicker.zeroOffset());
    }

    private static BandwidthLimiter create(double permitsPerSecond, SleepingTicker ticker) {
        Bandwidth bandwidth = Bandwidth.bursty(permitsPerSecond, ticker.elapsedMicros());
        return BandwidthLimiter.of(Bandwidths.of(bandwidth), ticker);
    }

    private static BandwidthLimiter create(double permitsPerSecond, long warmupPeriod, TimeUnit unit) {
        SleepingTicker ticker = SleepingTicker.zeroOffset();
        return create(permitsPerSecond, warmupPeriod, unit, 3.0, ticker);
    }

    private static BandwidthLimiter create(double permitsPerSecond, long warmupPeriod, TimeUnit unit,
                                   double coldFactor, SleepingTicker ticker) {
        Bandwidth bandwidth = Bandwidth
                .warmingUp(permitsPerSecond, ticker.elapsedMicros(), warmupPeriod, unit, coldFactor);
        return BandwidthLimiter.of(Bandwidths.of(bandwidth), ticker);
    }
    
    private long measureTotalTimeMillis(BandwidthLimiter limiter, int permits, Random random) {
        long startTime = ticker.instant;
        while (permits > 0) {
            int nextPermitsToAcquire = Math.max(1, random.nextInt(permits));
            permits -= nextPermitsToAcquire;
            limiter.acquire(nextPermitsToAcquire);
        }
        limiter.acquire(1); // to repay for any pending debt
        return NANOSECONDS.toMillis(ticker.instant - startTime);
    }

    private void assertEvents(String... events) {
        Assertions.assertEquals(Arrays.toString(events), ticker.readEventsAndClear());
    }

    /**
     * The ticker gathers events and presents them as strings. R0.6 means a delay of 0.6 seconds
     * caused by the (R)ateLimiter U1.0 means the (U)ser caused the ticker to sleep for a second.
     */
    static class FakeTicker implements SleepingTicker {
        long instant = 0L;
        final List<String> events = new ArrayList<>();

        @Override
        public long elapsedNanos() {
            return instant;
        }

        void sleepMillis(int millis) {
            sleepMicros("U", MILLISECONDS.toMicros(millis));
        }

        void sleepMicros(String caption, long micros) {
            instant += MICROSECONDS.toNanos(micros);
            events.add(caption + String.format(Locale.ROOT, "%3.2f", (micros / 1000000.0)));
        }

        @Override
        public void sleepMicrosUninterruptibly(long sleepFor) {
            sleepMicros("R", sleepFor);
        }

        String readEventsAndClear() {
            try {
                return events.toString();
            } finally {
                events.clear();
            }
        }

        @Override
        public String toString() {
            return events.toString();
        }
    }

    /*
     * Note: Mockito appears to lose its ability to Mock doGetRate as of Android 21. If we start
     * testing with that version or newer, we'll need to suppress this test (or see if Mockito can be
     * changed to support this).
     */
    @Test
    public void testMockingMockito() throws Exception {
        BandwidthLimiter mock = Mockito.mock(BandwidthLimiter.class);
        doTestMocking(mock);
    }

    private static void doTestMocking(BandwidthLimiter mock) throws Exception {
        for (Method method : BandwidthLimiter.class.getMethods()) {
            if (!isStatic(method.getModifiers())
                    && !NOT_WORKING_ON_MOCKS.contains(method.getName())
                    && !method.getDeclaringClass().equals(Object.class)) {
                method.invoke(mock, arbitraryParameters(method));
            }
        }
    }

    private static Object[] arbitraryParameters(Method method) {
        Class<?>[] parameterTypes = method.getParameterTypes();
        Object[] params = new Object[parameterTypes.length];
        for (int i = 0; i < parameterTypes.length; i++) {
            params[i] = PARAMETER_VALUES.get(parameterTypes[i]);
        }
        return params;
    }

    private static void assertTrue(boolean expression) {
        assertTrue("Expected: true, found: " + expression, expression);
    }

    private static void assertTrue(String message, boolean expression) {
        if (!expression) {
            throw new AssertionError(message);
        }
    }

    private static void assertFalse(boolean expression) {
        assertFalse("Expected: false, found: " + expression, expression);
    }

    private static void assertFalse(String message, boolean expression) {
        if (expression) {
            throw new AssertionError(message);
        }
    }

    private static void assertEquals(double lhs, double rhs) {
        Assertions.assertEquals(lhs, rhs);
    }

    private static void assertEquals(double lhs, double rhs, double epsilon) {
        Assertions.assertEquals(lhs, rhs, epsilon);
    }

    private static final Set<String> NOT_WORKING_ON_MOCKS =
            Collections.unmodifiableSet(new HashSet<>(
                    Arrays.asList("latestPermitAgeSec", "latestPermitAge", "setPermitsPerSecond", "getAvailablePermits")
            ));

    // We would use ArbitraryInstances, but it returns 0, invalid for many BandwidthLimiter methods.
    private static final Map<Class<?>, Object> PARAMETER_VALUES;
    static{
        Map<Class<?>, Object> m = new HashMap<>();
        m.put(int.class, 1);
        m.put(long.class, 1L);
        m.put(double.class, 1.0);
        m.put(TimeUnit.class, SECONDS);
        PARAMETER_VALUES = Collections.unmodifiableMap(m);
    }
}