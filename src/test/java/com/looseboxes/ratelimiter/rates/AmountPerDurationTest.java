package com.looseboxes.ratelimiter.rates;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

class AmountPerDurationTest {

    @Test
    void negativeAmountIsRejected() {
        Assertions.assertThrows(
                IllegalArgumentException.class,
                () -> Rate.of(-1, 0));
    }

    @Test
    void negativeDurationIsRejected() {
        Assertions.assertThrows(
                IllegalArgumentException.class,
                () -> Rate.of(1, -1));
    }

    @Test
    void amountIsIncremented() {
        final int amount = 1;
        Rate rate = Rate.of(amount, 1000);
        final int increment = 4;
        Rate incremented = rate.increment(increment);
        assertThat(((AmountPerDuration)incremented).getAmount()).isEqualTo(amount + increment);
    }

    @Test
    void durationIsIncrementedByTimeElapsed() throws InterruptedException{
        Rate rate = Rate.of(1, 0);
        final long timeElapsed = 100;
        Thread.sleep(timeElapsed);
        Rate incremented = rate.increment();
        final long expectedAtMost = System.currentTimeMillis() - ((AmountPerDuration)rate).getTimeCreated();
        //System.out.println("Duration: " + incremented.getDuration() + ", expected: " + expectedAtMost);
        assertThat(((AmountPerDuration)incremented).getDuration()).isEqualTo(expectedAtMost);
    }
}
