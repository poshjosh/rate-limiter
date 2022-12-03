package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;
import org.assertj.core.api.Condition;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class RateLimiterTest {

    private final String key = "0";

    private final int durationMillis = 1_500;

    @Test
    void shouldNotBeAffectedByLongInitialDelay() throws InterruptedException {
        final long duration = 100;
        RateLimiter<String> rateLimiter = RateLimiter.of(Rate.of(1, duration));
        Thread.sleep(duration + 1);
        assertTrue(rateLimiter.consume(key), "Unable to acquire initial permit");
        assertFalse(rateLimiter.consume(key), "Capable of acquiring additional permit");
    }

    @Test
    void veryLargeLimitShouldNotBeAffectedByDuration() {
        final long duration = 1;
        RateLimiter<String> rateLimiter = RateLimiter.of(Rate.of(Long.MAX_VALUE, duration));
        for (int i = 0; i < 100; i++) {
            assertTrue(rateLimiter.consume(key), "Unable to acquire permit " + i);
        }
    }

    @Test
    void immediateConsumeShouldSucceed() {
        RateLimiter<String> rateLimiter = perSecondRateLimiter(1);
        assertTrue(rateLimiter.consume(key), "Unable to acquire initial permit");
        assertFalse(rateLimiter.consume(key), "Capable of acquiring additional permit");
    }

    @Test
    void testConsumeParameterValidation() {
        RateLimiter<String> rateLimiter = perSecondRateLimiter(999);
        //assertThrowsRuntimeException(() -> rateLimiter.consume(null, id, 1));
        assertThrowsRuntimeException(() -> rateLimiter.consume(key, -1));
        assertThrowsRuntimeException(() -> rateLimiter.consume(null, 1));
    }

    @Test
    void testNewInstanceParameterValidation() {
        assertThrowsRuntimeException(() -> RateLimiter.of(Rate.of(-1, 1)));
        assertThrowsRuntimeException(() -> RateLimiter.of(Rate.of(1, -1)));
    }

    private static void assertTrue(boolean expression, String message) {
        assertThat(expression).is(new Condition<>(b -> b, message));
    }

    private static void assertFalse(boolean expression, String message) {
        assertThat(expression).is(new Condition<>(b -> !b, message));
    }

    private static void assertThrowsRuntimeException(Executable executable) {
        assertThrows(RuntimeException.class, executable);
    }

    private <T> RateLimiter<T> perSecondRateLimiter(long amount) {
        return RateLimiter.of(Rate.of(amount, durationMillis));
    }

    @Test
    void shouldResetWhenAtThreshold() throws Exception{
        RateLimiter<String> rateLimiter = getRateLimiter(getLimitsThatWillLeadToReset());
        rateLimiter.consume(key);

        // Simulate some time before the next recording
        // This way we can have a reset
        Thread.sleep(durationMillis + 500);

        rateLimiter.consume(key);
    }

    @Test
    void shouldFailWhenLimitExceeded() {
        RateLimiter<String> rateLimiter = getRateLimiter(getDefaultLimits());
        rateLimiter.consume(key);
        assertThat(rateLimiter.consume(key)).isFalse();
    }

    public RateLimiter<String> getRateLimiter(Rate... rates) {
        return RateLimiter.of(rates);
    }

    protected Rate [] getDefaultLimits() { return new Rate[]{getDefaultLimit()}; }

    protected List<Rate> getLimitsThatWillLeadToException() {
        return Arrays.asList(getBaseRate(), getDefaultLimit());
    }

    protected Rate getDefaultLimit() {
        return getRate(1, durationMillis);
    }

    protected Rate [] getLimitsThatWillLeadToReset() {
        return new Rate [] {getBaseRate(), getBaseRate()};
    }

    private Rate getBaseRate() {
        return getRate(1, 0);
    }

    private Rate getRate(long amount, long duration) {
        return Rate.of(amount, duration);
    }
}