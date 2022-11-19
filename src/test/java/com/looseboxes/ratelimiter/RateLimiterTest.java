package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.AmountPerDuration;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateConfigList;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.assertThatThrownBy;

public class RateLimiterTest {

    private final int durationMillis = 1_500;

    private RateLimiter<String> rateLimiter;

    @BeforeEach
    void setUp() {
        rateLimiter = getRateLimiter(getDefaultLimits());
    }

    @Test
    void shouldResetWhenAtThreshold() throws Exception{
        rateLimiter = getRateLimiter(getLimitsThatWillLeadToReset());
        final String key = getKey(0);
        rateLimiter.increment(key);

        // Simulate some time before the next recording
        // This way we can have a reset
        Thread.sleep(durationMillis + 500);

        rateLimiter.increment(key);
    }

    @Test
    void shouldFailWhenLimitExceeded() {
        final String key = getKey(0);
        rateLimiter.increment(key);
        assertThatThrownBy(() -> rateLimiter.increment(key));
    }

    protected void assertEquals(Rate result, RateConfig expected) {
        AmountPerDuration rate = (AmountPerDuration)result;
        assertThat(rate.getAmount()).isEqualTo(expected.getLimit());
        assertThat(rate.getDuration()).isEqualTo(expected.getDuration().toMillis());
    }

    public RateLimiter<String> getRateLimiter(List<RateConfig> limits) {
        return new SimpleRateLimiter<>(new RateConfigList().addLimits(limits));
    }

    protected String getKey(int index) {
        return Integer.toString(index + 1);
    }

    protected List<RateConfig> getDefaultLimits() { return Arrays.asList(getDefaultLimit()); }

    protected List<RateConfig> getLimitsThatWillLeadToException() {
        return Arrays.asList(getBaseRate(), getDefaultLimit());
    }

    protected RateConfig getDefaultLimit() {
        return new RateConfig().limit(1).duration(Duration.ofMillis(durationMillis));
    }

    protected List<RateConfig> getLimitsThatWillLeadToReset() {
        return Arrays.asList(getBaseRate(), getBaseRate());
    }

    private RateConfig getBaseRate() {
        return new RateConfig().limit(1).duration(Duration.ZERO);
    }
}