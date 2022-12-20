package com.looseboxes.ratelimiter.rates;

import com.looseboxes.ratelimiter.Rate;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class RateTest {

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
}
