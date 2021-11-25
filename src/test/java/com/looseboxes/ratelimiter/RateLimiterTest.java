package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.LimitWithinDuration;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateLimitConfig;
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
        LimitWithinDuration result = (LimitWithinDuration)instance.record(key);
        RateConfig expected = getBaseRate();
        assertThat(result.getLimit()).isEqualTo(expected.getLimit());
        assertThat(result.getDuration()).isEqualTo(expected.getTimeUnit().toMillis(expected.getDuration()));
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
        final String key = getKey(0);
        Rate result = instance.record(key);
        assertThatThrownBy(() -> instance.record(key));
    }

    public RateLimiter<Object> getRateLimiter(List<RateConfig> limits) {
        return new DefaultRateLimiter<>(new RateLimitConfig().addLimits(limits));
    }

    protected String getKey(int index) {
        return Integer.toString(index + 1);
    }

    protected List<RateConfig> getDefaultLimits() { return Arrays.asList(getDefaultLimit()); }

    protected List<RateConfig> getLimitsThatWillLeadToException() {
        return Arrays.asList(getBaseRate(), getDefaultLimit());
    }

    protected RateConfig getDefaultLimit() {
        return new RateConfig().limit(1).duration(durationMillis);
    }

    protected List<RateConfig> getLimitsThatWillLeadToReset() {
        return Arrays.asList(getBaseRate(), getBaseRate());
    }

    private RateConfig getBaseRate() {
        return new RateConfig().limit(1).duration(0);
    }
}