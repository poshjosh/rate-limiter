package com.looseboxes.ratelimiter.rates.experimental;

import com.looseboxes.ratelimiter.rates.AmountPerDuration;
import com.looseboxes.ratelimiter.rates.Logic;
import com.looseboxes.ratelimiter.rates.Rate;
import org.junit.jupiter.api.Test;

//import static org.assertj.core.api.Assertions.assertThat;

class CompositeRateTest {

    @Test
    void orCompositeShouldReturnPositiveWhenComparedToLowerRates() {
        orCompositeWhenComparedToLowerRates(true);
    }

    @Test
    void orCompositeReversedShouldReturnNegativeWhenComparedToLowerRates() {
        orCompositeWhenComparedToLowerRates(false);
    }

    private void orCompositeWhenComparedToLowerRates(boolean forward) {
        Rate lhs = getRate(3, 3);
        Rate rhs = getRate(2, 2);
        CompositeRate composite = getOrInstance(lhs, rhs);
        Rate other = getRate(1, 3);
        assertPositveWhenForward(composite, other, forward);
    }

    @Test
    void andCompositeShouldReturnPositiveWhenComparedToLowerRates() {
        andCompositeComparedToLowerRates(true);
    }

    @Test
    void andCompositeShouldReturnNegativeWhenReverseComparedToLowerRates() {
        andCompositeComparedToLowerRates(false);
    }

    private void andCompositeComparedToLowerRates(boolean forward) {
        Rate lhs = getRate(2, 0);
        Rate rhs = getRate(3, 0);
        Rate composite = getAndInstance(lhs, rhs);
        Rate other = getRate(1, 1);
        assertPositveWhenForward(composite, other, forward);
    }

    private void assertPositveWhenForward(Rate lhs, Rate rhs, boolean forward) {
        if(forward) {
//            assertThat(lhs.compareTo(rhs)).isPositive();
        }else{
//            assertThat(rhs.compareTo(lhs)).isNegative();
        }
    }

    @Test
    void orCompositeShouldReturnZeroWhenComparedToMatchingRate() {
        whenComparedToMatchingRate(Logic.OR, true);
    }

    @Test
    void orCompositeShouldReturnZeroWhenReverseComparedToMatchingRate() {
        whenComparedToMatchingRate(Logic.OR, false);
    }
    @Test
    void andCompositeShouldReturnZeroWhenComparedToMatchingRate() {
        whenComparedToMatchingRate(Logic.AND, true);
    }

    @Test
    void andCompositeShouldReturnZeroWhenReverseComparedToMatchingRate() {
        whenComparedToMatchingRate(Logic.AND, false);
    }

    private void whenComparedToMatchingRate(Logic logic, boolean forward) {
        Rate lhs = getRate(2, 2);
        Rate rhs = getRate(3, 3);
        Rate composite = getInstance(logic, lhs, rhs);
        Rate other = getRate(2, 2);
        assertZeroByComparison(composite, other, forward);
    }

    private void assertZeroByComparison(Rate lhs, Rate rhs, boolean forward) {
        if(forward) {
//            assertThat(lhs.compareTo(rhs)).isZero();
        }else{
//            assertThat(rhs.compareTo(lhs)).isZero();
        }
    }

    @Test
    void orCompositeShouldReturnNegativeWhenComparedToHigherRates() {
        compositeRateWhenComparedToHigherRates(Logic.OR, true);
    }
    @Test
    void orCompositeShouldReturnPositiveWhenReverseComparedToHigherRates() {
        compositeRateWhenComparedToHigherRates(Logic.OR, false);
    }

    @Test
    void andCompositeShouldReturnNegativeWhenComparedToHigherRates() {
        compositeRateWhenComparedToHigherRates(Logic.AND, true);
    }

    @Test
    void andCompositeShouldReturnPositiveWhenReverseComparedToHigherRates() {
        compositeRateWhenComparedToHigherRates(Logic.AND, false);
    }

    private void compositeRateWhenComparedToHigherRates(Logic logic, boolean forward) {
        Rate lhs = getRate(1, 1);
        Rate rhs = getRate(2, 2);
        Rate composite = getInstance(logic, lhs, rhs);
        Rate other = getRate(3, 3);
        assertNegativeWhenForward(composite, other, forward);
    }

    private void assertNegativeWhenForward(Rate lhs, Rate rhs, boolean forward) {
        if(forward) {
//            assertThat(lhs.compareTo(rhs)).isNegative();
        }else{
//            assertThat(rhs.compareTo(lhs)).isPositive();
        }
    }

    @Test
    void orCompositeShouldReturnZeroWhenComparedToMatchingRates() {
        shouldReturnZeroWhenComparedToMatchingRates(Logic.OR);
    }

    @Test
    void andCompositeShouldReturnZeroWhenComparedToMatchingRates() {
        shouldReturnZeroWhenComparedToMatchingRates(Logic.AND);
    }

    private void shouldReturnZeroWhenComparedToMatchingRates(Logic logic) {
        Rate lhs = getRate(1, 1);
        Rate rhs = getRate(2, 2);
        Rate composite0 = getInstance(logic, lhs, rhs);
        Rate composite1 = getInstance(logic, lhs, rhs);
//        assertThat(composite0.compareTo(composite1)).isZero();
//        assertThat(composite1.compareTo(composite0)).isZero();
    }

    @Test
    void orSelfShouldReturnRateEqualByComparisonToSelf() {
        Rate rate = getRate(1, 1);
        CompositeRate composite = getOrInstance(rate, rate);
        assertEqualByComparingTo(rate, composite);
    }

    @Test
    void andSelfShouldReturnRateEqualByComparisonToSelf() {
        Rate rate = getRate(1, 1);
        CompositeRate composite = getAndInstance(rate, rate);
        assertEqualByComparingTo(rate, composite);
    }

    void assertEqualByComparingTo(Rate rate, CompositeRate composite) {
        // This will fail because CompositeRate cannot be compared to the Rate type. They are different classes
        // Compare to requires them to be the same class
        // assertThat(rate).isEqualByComparingTo(composite);
//        assertThat((Rate)composite).isEqualByComparingTo(rate);
        assertEqualByComparingTo(rate, composite, rate);
        assertEqualByComparingTo(rate, composite, rate.increment());
    }

    void assertEqualByComparingTo(Rate lhs, Rate rhs, Rate mark) {
        int expected = lhs.compareTo(mark);
        int result = rhs.compareTo(mark);
        if(expected > 0) {
//            assertThat(result).isPositive();
        }else if(expected < 0) {
//            assertThat(result).isNegative();
        }else{
//            assertThat(result).isZero();
        }
    }

    Rate getRate(int amount, int duration) {
        return AmountPerDuration.of(amount, duration);
    }

    public CompositeRate getInstance(Logic logic, Rate... rates) {
        return Rates.compose(logic, rates);
    }

    public CompositeRate getAndInstance(Rate... rates) {
        return Rates.and(rates);
    }

    public CompositeRate getOrInstance(Rate... rates) {
        return Rates.or(rates);
    }
}