package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.InMemoryRateCache;
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

    private RateLimiterConfiguration<Object> rateLimiterConfiguration;
    private RateLimiter rateLimiter;

    @BeforeEach
    void setUp() {
        rateLimiterConfiguration = new RateLimiterConfiguration<>()
                .rateCache(new InMemoryRateCache<>())
                .rateFactory(new LimitWithinDurationFactory())
                .rateExceededListener(new RateExceededExceptionThrower());
        rateLimiter = getRateLimiter(getDefaultLimits());
    }

    @Test
    void shouldResetWhenAtThreshold() throws Exception{
        rateLimiter = getRateLimiter(getLimitsThatWillLeadToReset());
        final String key = getKey(0);
        incrementForResult(key);

        // Simulate some time before the next recording
        // This way we can have a reset
        Thread.sleep(durationMillis + 500);

        incrementForResult(key);
    }

    @Test
    void shouldFailWhenLimitExceeded() {
        final String key = getKey(0);
        rateLimiter.increment(key);
        assertThatThrownBy(() -> rateLimiter.increment(key));
    }

    private Rate incrementForResult(String key) {
        rateLimiter.increment(key);
        return rateLimiterConfiguration.getRateCache().get(key);
    }

    protected void assertEquals(Rate result, RateConfig expected) {
        LimitWithinDuration rate = (LimitWithinDuration)result;
        assertThat(rate.getLimit()).isEqualTo(expected.getLimit());
        assertThat(rate.getDuration()).isEqualTo(expected.getTimeUnit().toMillis(expected.getDuration()));
    }

    public RateLimiter<Object> getRateLimiter(List<RateConfig> limits) {
        return new SimpleRateLimiter<>(new RateLimitConfig().addLimits(limits));
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