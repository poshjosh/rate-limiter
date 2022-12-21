package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

abstract class AbstractRateLimiterTest {

    final String key = "0";

    final int durationMillis = 1_500;

    private final BandwidthFactory bandwidthFactory;
    private final boolean supportsNullKeys;

    AbstractRateLimiterTest(BandwidthFactory bandwidthFactory, boolean supportsNullKeys) {
        this.bandwidthFactory = Objects.requireNonNull(bandwidthFactory);
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

    // TODO - Find out why this fails for values <= 1000 (This may be the expected behaviour
    @ParameterizedTest
    @ValueSource(longs = {2_000, /** 100 */})
    void shouldExceedLimitAfterLongInitialDelay(long duration) throws InterruptedException {
        RateLimiter<String> rateLimiter = getRateLimiter(getRate(1, duration));
        Thread.sleep(duration + 1);
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

    @Test
    void shouldResetWhenLimitNotExceededWithinDuration() throws InterruptedException{
        final long limit = 1; // TODO - Fails for limits > 1
        final long duration = 1000;
        RateLimiter<String> rateLimiter = getRateLimiter(getRate(limit, duration));

        for (int i = 0; i < limit; i++) {
            assertTrue(rateLimiter.tryConsume(key), "Unable to acquire permit " + i);
        }
        assertFalse(rateLimiter.tryConsume(key), "Capable of acquiring permit " + (limit + 1));

        Thread.sleep(duration); // Leads to reset

        for (int i = 0; i < limit; i++) {
            assertTrue(rateLimiter.tryConsume(key), "Unable to acquire permit " + i);
        }
        assertFalse(rateLimiter.tryConsume(key), "Capable of acquiring permit " + (limit + 1));
    }

    @Test
    void shouldResetWhenAtThreshold() throws Exception{
        RateLimiter<String> rateLimiter = getRateLimiter(getLimitsThatWillLeadToReset());
        rateLimiter.tryConsume(key);

        // Simulate some time before the next recording
        // This way we can have a reset
        Thread.sleep(durationMillis + 500);

        rateLimiter.tryConsume(key);
    }

    @Test
    void shouldFailWhenLimitExceeded() {
        RateLimiter<String> rateLimiter = getRateLimiter(getDefaultLimits());
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

    public <T> RateLimiter<T> getRateLimiter(Bandwidth... rates) {
        return RateLimiter.of(rates);
    }

    protected Bandwidth [] getDefaultLimits() { return new Bandwidth[]{getDefaultLimit()}; }

    protected Bandwidth getDefaultLimit() {
        return getRate(1, durationMillis);
    }

    protected Bandwidth [] getLimitsThatWillLeadToReset() {
        return new Bandwidth[] {getBaseRate()};
    }

    protected Bandwidth getBaseRate() {
        return getRate(1, 0);
    }

    protected Bandwidth getRate(long amount, long durationMillis) {
        return bandwidthFactory.createNew(amount, durationMillis, TimeUnit.MILLISECONDS);
    }
}
