package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.AmountPerDuration;
import com.looseboxes.ratelimiter.rates.Rate;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.assertThatThrownBy;

public class RateLimiterTest {

    private final int durationMillis = 1_500;

    @Test
    void shouldResetWhenAtThreshold() throws Exception{
        RateLimiter<String> rateLimiter = getRateLimiter(getLimitsThatWillLeadToReset());
        final String key = "0";
        rateLimiter.consume(key);

        // Simulate some time before the next recording
        // This way we can have a reset
        Thread.sleep(durationMillis + 500);

        rateLimiter.consume(key);
    }

    @Test
    void shouldFailWhenLimitExceeded() {
        RateLimiter<String> rateLimiter = getRateLimiter(getDefaultLimits());
        final String key = "0";
        rateLimiter.consume(key);
        assertThat(rateLimiter.consume(key)).isFalse();
    }

    public RateLimiter<String> getRateLimiter(Rate... rates) {
        return new SimpleRateLimiter<>(rates);
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
        return AmountPerDuration.of(amount, duration);
    }
}