package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.LimitWithinDuration;
import com.looseboxes.ratelimiter.rates.Rate;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.assertThatThrownBy;

public class RateLimiterTest {

    private final int durationMillis = 1_500;

    private RateLimiter instance;

    @BeforeEach
    void setUp() {
        instance = getRateLimiter(getDefaultLimits());
    }

    @Test
    void firstRateShouldEqualBaseRate() {
        final String key = getKey(0);
        Rate result = instance.record(key);
        assertThat(result).isEqualTo(getBaseRate());
    }

    @Test
    void firstRateShouldBeLessThanHigherRate() {
        final String key = getKey(0);
        instance.record(key);
        assertThatThrownBy(() -> instance.record(key));
    }

    @Test
    void shouldResetWhenAtThreshold() throws Exception{
        instance = getRateLimiter(getLimitsThatWillLeadToReset());
        final String key = getKey(0);
        Rate result = instance.record(key);

        // Simulate some time before the next recording
        // This way we can have a reset
        Thread.sleep(durationMillis + 500);

        result = instance.record(key);
        assertThat(result).isEqualTo(Rate.NONE);
    }

    @Test
    void shouldFailWhenLimitExceeded() {
        instance = getRateLimiter(getLimitsThatWillLeadToException());
        final String key = getKey(0);
        Rate result = instance.record(key);
        assertThatThrownBy(() -> instance.record(key));
    }

    public RateLimiter getRateLimiter(List<Rate> limits) {
        return new RateLimiterImpl(getBaseRateSupplier(), limits);
    }

    protected String getKey(int index) {
        return Integer.toString(index + 1);
    }

    protected List<Rate> getDefaultLimits() { return Arrays.asList(getDefaultLimit()); }

    protected List<Rate> getLimitsThatWillLeadToException() {
        return Arrays.asList(getBaseRate(), getDefaultLimit());
    }

    protected Rate getDefaultLimit() {
        return new LimitWithinDuration(1, durationMillis);
    }

    protected List<Rate> getLimitsThatWillLeadToReset() {
        return Arrays.asList(getBaseRate(), getBaseRate());
    }

    protected RateSupplier getBaseRateSupplier() {
        return () -> getBaseRate();
    }

    private Rate getBaseRate() {
        return new LimitWithinDuration();
    }
}