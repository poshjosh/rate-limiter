package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.Rate;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.time.Duration;
import java.util.Objects;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

abstract class AbstractRateLimiterTest {

    final String key = "0";

    final int durationMillis = 2000;

    private final Class<? extends BandwidthFactory> factoryClass;
    private final boolean supportsNullKeys;

    AbstractRateLimiterTest(Class<? extends BandwidthFactory> factoryClass, boolean supportsNullKeys) {
        this.factoryClass = Objects.requireNonNull(factoryClass);
        this.supportsNullKeys = supportsNullKeys;
    }

    @ParameterizedTest
    @ValueSource(longs = {2_000, 100})
    void shouldNotBeAffectedByLongInitialDelay() throws InterruptedException {
        final long duration = 100;
        RateLimiter<String> rateLimiter = getRateLimiter(getRate(2, duration));
        Thread.sleep(duration + 1);
        assertTrue(rateLimiter.tryConsume(key), "Unable to acquire initial permit");
    }

    @ParameterizedTest
    @ValueSource(longs = {2_000, 100})
    void shouldExceedLimitAfterLongInitialDelay(long duration) throws InterruptedException {
        RateLimiter<String> rateLimiter = getRateLimiter(getRate(1, duration));
        Thread.sleep(duration + 10);
        assertTrue(rateLimiter.tryConsume(key), "Unable to acquire initial permit");
        assertFalse(rateLimiter.tryConsume(key), "Capable of acquiring additional permit");
    }

    @Test
    void veryLargeLimitShouldNotBeAffectedByDuration() {
        final long duration = 1;
        RateLimiter<String> rateLimiter = getRateLimiter(getRate(Long.MAX_VALUE, duration));
        for (int i = 0; i < 100; i++) {
            assertTrue(rateLimiter.tryConsume(key), "Unable to acquire permit " + i);
        }
    }

    @Test
    void immediateConsumeShouldSucceed() {
        RateLimiter<String> rateLimiter = perSecondRateLimiter(1);
        assertTrue(rateLimiter.tryConsume(key), "Unable to acquire initial permit");
    }

    @Test
    void testConsumeParameterValidation() {
        RateLimiter<String> rateLimiter = perSecondRateLimiter(999);
        assertThrowsRuntimeException(() -> rateLimiter.tryConsume(key, -1));
        if (!supportsNullKeys) {
            assertThrowsRuntimeException(() -> rateLimiter.tryConsume(null, 1));
        }
    }

    protected <T> RateLimiter<T> perSecondRateLimiter(long amount) {
        return getRateLimiter(getRate(amount, durationMillis));
    }

    @Test
    void testNewInstanceParameterValidation() {
        assertThrowsRuntimeException(() -> getRateLimiter(getRate(-1, 1)));
        assertThrowsRuntimeException(() -> getRateLimiter(getRate(1, -1)));
    }

    @ParameterizedTest
    @ValueSource(longs = {1, 2, 4})
    void shouldResetWhenLimitNotExceededWithinDuration(long limit) throws InterruptedException{
        final long duration = 2000;
        RateLimiter<String> rateLimiter = getRateLimiter(getRate(limit, duration));

        for (int i = 0; i < limit; i++) {
            //System.out.println(i);
            assertTrue(rateLimiter.tryConsume(key), "Unable to acquire permit " + i);
        }
        //System.out.println();
        assertFalse(rateLimiter.tryConsume(key), "Capable of acquiring permit " + (limit + 1));

        Thread.sleep(duration); // Leads to reset

        for (int i = 0; i < limit; i++) {
            //System.out.println(i);
            assertTrue(rateLimiter.tryConsume(key), "Unable to acquire permit " + i);
        }
        //System.out.println();
        assertFalse(rateLimiter.tryConsume(key), "Capable of acquiring permit " + (limit + 1));
    }

    @Test
    void shouldResetWhenAtThreshold() throws Exception{
        RateLimiter<String> rateLimiter = getRateLimiter(getRate(1, 0));
        rateLimiter.tryConsume(key);

        // Simulate some time before the next recording
        // This way we can have a reset
        Thread.sleep(durationMillis + 500);

        rateLimiter.tryConsume(key);
    }

    @Test
    void shouldFailWhenLimitExceeded() {
        RateLimiter<String> rateLimiter = getRateLimiter(getRate(2, 1000));
        assertThat(rateLimiter.tryConsume(key)).isTrue();
        assertThat(rateLimiter.tryConsume(key)).isTrue();
        assertThat(rateLimiter.tryConsume(key)).isFalse();
    }

    static void assertTrue(boolean expression, String message) {
        assertThat(expression).withFailMessage(message).isTrue();
    }

    static void assertFalse(boolean expression, String message) {
        assertThat(expression).withFailMessage(message).isFalse();
    }

    static void assertThrowsRuntimeException(Executable executable) {
        assertThrows(RuntimeException.class, executable);
    }

    public <T> RateLimiter<T> getRateLimiter(Rate... rates) {
        return RateLimiter.of(rates);
    }

    protected Rate getRate(long permits, long durationMillis) {
        return Rate.of(permits, Duration.ofMillis(durationMillis), factoryClass);
    }
}
