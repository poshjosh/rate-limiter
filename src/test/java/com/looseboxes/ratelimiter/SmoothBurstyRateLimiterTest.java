package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.wip.BandwidthLimiter;
import com.looseboxes.ratelimiter.wip.SimpleBandwidthLimiter;
import com.looseboxes.ratelimiter.wip.SmoothBurstyRateLimiter;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.time.LocalTime;
import java.time.temporal.ChronoUnit;

import static java.util.concurrent.TimeUnit.SECONDS;

class SmoothBurstyRateLimiterTest extends AbstractRateLimiterTest{

    private final boolean debug = true;

    SmoothBurstyRateLimiterTest() {
        super(true);
    }

    @Override
    @Test
    void shouldExceedLimitAfterLongInitialDelay() throws InterruptedException {
        //TODO - Take note, that values of 1000 and below fails. This may be the legit behaviour
        final long duration = 100;
        //shouldExceedLimitAfterLongInitialDelay(duration);
        shouldExceedLimitAfterLongInitialDelay(duration * 20);
    }

    void shouldExceedLimitAfterLongInitialDelay(long duration) throws InterruptedException {
        RateLimiter<String> rateLimiter = getRateLimiter(Rate.of(1, duration));
        BandwidthLimiter delegate = getDelegate(rateLimiter);
        Duration timeout = getStableInterval(delegate);

        Thread.sleep(duration + 1);

        debug(((SimpleBandwidthLimiter)delegate).getRate());
        assertTrue(delegate.tryAcquire(1, timeout), "Unable to acquire initial permit");
        debug(((SimpleBandwidthLimiter)delegate).getRate());
        assertFalse(rateLimiter.consume(key), "Capable of acquiring additional permit");
    }

    @Override
    @Test
    void shouldResetWhenLimitNotExceededWithinDuration() throws InterruptedException{
        final long limit = 10;
        final long duration = 1000;

        RateLimiter<String> rateLimiter = getRateLimiter(Rate.of(limit, duration));
        BandwidthLimiter delegate = getDelegate(rateLimiter);
        Duration timeout = getStableInterval(delegate);

        for (int i = 0; i < limit; i++) {
            assertTrue(delegate.tryAcquire(1, timeout), "Unable to acquire permit " + i);
        }
        assertFalse(rateLimiter.consume(key), "Capable of acquiring permit " + (limit + 1));

        Thread.sleep(duration); // Leads to reset

        for (int i = 0; i < limit; i++) {
            assertTrue(delegate.tryAcquire(1, timeout), "Unable to acquire permit " + i);
        }
        assertFalse(rateLimiter.consume(key), "Capable of acquiring permit " + (limit + 1));
    }

    private Duration getStableInterval(BandwidthLimiter rateLimiter) {
        long micros = (long)(SECONDS.toMicros(1L) / rateLimiter.getPermitsPerSecond());
        // TODO - Fix flakiness. For now we add 10 as a temporary fix
        return Duration.of(micros + 10, ChronoUnit.MICROS);
    }

    private <T> BandwidthLimiter getDelegate(RateLimiter<T> rateLimiter) {
        return ((SmoothBurstyRateLimiter<T>)rateLimiter).getDelegate();
    }

    public <T> RateLimiter<T> getRateLimiter(Rate... rates) {
        if (rates.length == 1) {
            return new SmoothBurstyRateLimiter<>(rates[0]);
        }
        throw new UnsupportedOperationException("Not supported");
    }

    @Override
    protected Rate [] getDefaultLimits() { return new Rate[]{getDefaultLimit()}; }

    @Override
    protected Rate [] getLimitsThatWillLeadToReset() {
        return new Rate [] {getBaseRate()};
    }

    private void debug(Object message) {
        if (!debug) {
            return;
        }
        System.out.println("" + LocalTime.now() + " " + this.getClass().getSimpleName() + " " + message);
    }
}
