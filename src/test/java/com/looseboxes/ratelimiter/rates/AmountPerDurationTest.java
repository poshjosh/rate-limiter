package com.looseboxes.ratelimiter.rates;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

class AmountPerDurationTest {

    @Test
    void negativeAmountIsRejected() {
        Assertions.assertThrows(
                IllegalArgumentException.class,
                () -> AmountPerDuration.of(-1, 0));
    }

    @Test
    void negativeDurationIsRejected() {
        Assertions.assertThrows(
                IllegalArgumentException.class,
                () -> AmountPerDuration.of(1, -1));
    }

    @Test
    void amountIsIncremented() {
        final int amount = 1;
        AmountPerDuration amountPerDuration = AmountPerDuration.of(amount, 1000);
        final int increment = 4;
        AmountPerDuration incremented = amountPerDuration.increment(increment);
        assertThat(incremented.getAmount()).isEqualTo(amount + increment);
    }

    @Test
    void durationIsIncrementedByTimeElapsed() throws InterruptedException{
        AmountPerDuration amountPerDuration = AmountPerDuration.of(1, 0);
        final long timeElapsed = 100;
        Thread.sleep(timeElapsed);
        AmountPerDuration incremented = amountPerDuration.increment();
        final long expectedAtMost = System.currentTimeMillis() - amountPerDuration.getTimeCreated();
        //System.out.println("Duration: " + incremented.getDuration() + ", expected: " + expectedAtMost);
        assertThat(incremented.getDuration()).isEqualTo(expectedAtMost);
    }
}
