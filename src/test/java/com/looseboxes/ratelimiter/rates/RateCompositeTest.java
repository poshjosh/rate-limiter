package com.looseboxes.ratelimiter.rates;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class RateCompositeTest {

    @Test
    void orCompositeShouldBeGreaterThanLowerRateByComparison() {
        Rate lhs = getRate(3, 3);
        Rate rhs = getRate(2, 2);
        Rate composite = getOrInstance(lhs, rhs);
        assertThat(composite.compareTo(getRate(1, 3))).isPositive();
    }

    @Test
    void andCompositeShouldBeGreaterThanLowerRateByComparison() {
        Rate lhs = getRate(2, 0);
        Rate rhs = getRate(3, 0);
        Rate composite = getOrInstance(lhs, rhs);
        assertThat(composite.compareTo(getRate(1, 1))).isPositive();
    }

    @Test
    void orCompositeShouldBeEqualToLowerRateByComparison() {
        Rate lhs = getRate(2, 2);
        Rate rhs = getRate(3, 3);
        Rate composite = getOrInstance(lhs, rhs);
        assertThat(composite.compareTo(getRate(2, 2))).isZero();
    }

    @Test
    void andCompositeShouldBeEqualToLowerRateByComparison() {
        Rate lhs = getRate(2, 2);
        Rate rhs = getRate(3, 3);
        Rate composite = getAndInstance(lhs, rhs);
        assertThat(composite.compareTo(getRate(2, 2))).isZero();
    }

    @Test
    void orCompositeShouldBeLessThanHigherRateByComparison() {
        Rate lhs = getRate(1, 1);
        Rate rhs = getRate(2, 2);
        Rate composite = getOrInstance(lhs, rhs);
        assertThat(composite.compareTo(getRate(3, 3))).isNegative();
    }

    @Test
    void andCompositeShouldBeLessThanHigherRateByComparison() {
        Rate lhs = getRate(1, 1);
        Rate rhs = getRate(2, 2);
        Rate composite = getAndInstance(lhs, rhs);
        assertThat(composite.compareTo(getRate(3, 3))).isNegative();
    }

    @Test
    void orCompositeShouldBeConsistentByComparison() {
        Rate lhs = getRate(1, 1);
        Rate rhs = getRate(2, 2);
        Rate composite0 = getOrInstance(lhs, rhs);
        Rate composite1 = getOrInstance(lhs, rhs);
        assertThat(composite0.compareTo(composite1)).isZero();
    }

    @Test
    void andCompositeShouldBeConsistentByComparison() {
        Rate lhs = getRate(1, 1);
        Rate rhs = getRate(2, 2);
        Rate composite0 = getAndInstance(lhs, rhs);
        Rate composite1 = getAndInstance(lhs, rhs);
        assertThat(composite0.compareTo(composite1)).isZero();
    }

    @Test
    void orSelfShouldReturnRateEqualByComparisonToSelf() {
        Rate rate = getRate(1, 1);
        Rate composite = getOrInstance(rate, rate);
        assertEqualByComparingTo(rate, composite);
    }

    @Test
    void andSelfShouldReturnRateEqualByComparisonToSelf() {
        Rate rate = getRate(1, 1);
        Rate composite = getAndInstance(rate, rate);
        assertEqualByComparingTo(rate, composite);
    }

    void assertEqualByComparingTo(Rate rate, Rate composite) {
        // This will fail because RateComposite cannot be compared to the Rate type. They are different classes
        // Compare to requires them to be the same class
        // assertThat(rate).isEqualByComparingTo(composite);
        assertThat(composite).isEqualByComparingTo(rate);
        assertEqualByComparingTo(rate, composite, rate);
        assertEqualByComparingTo(rate, composite, rate.increment());
    }

    void assertEqualByComparingTo(Rate lhs, Rate rhs, Rate mark) {
        int expected = lhs.compareTo(mark);
        int result = rhs.compareTo(mark);
        assertThat(result).isEqualTo(expected);
    }

    Rate getRate(int limit, int duration) {
        return new LimitWithinDuration(limit, duration);
    }

    public Rate getAndInstance(Rate... rates) {
        return Rates.and(rates);
    }

    public Rate getOrInstance(Rate... rates) {
        return Rates.or(rates);
    }
}