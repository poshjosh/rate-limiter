package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.model.Operator;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.assertThatThrownBy;

class BandwidthArrayTest {

    @Test
    void givenEmptyArray_shouldReturnUnlimited() {
        final Bandwidth bandwidth = BandwidthArray.of(Operator.OR, new Bandwidth[0]);
        assertThat(bandwidth).isSameAs(Bandwidths.UNLIMITED);
    }

    @Test
    void givenSingleUnlimitedBandwidth_shouldReturnSame() {
        final Bandwidth expected = Bandwidths.UNLIMITED;
        final Bandwidth result = BandwidthArray.of(Operator.OR, expected);
        assertThat(result).isSameAs(expected);
    }

    @Test
    void givenSingleLimitedBandwidth_shouldReturnEqual() {
        final Bandwidth expected = Bandwidths.ofSeconds(1);
        final Bandwidth result = BandwidthArray.of(Operator.OR, expected);
        assertThat(result).isEqualTo(expected);
    }

    @Test
    void givenMultipleUnlimitedBandwidths_shouldReturnSingleUnlimited() {
        final Bandwidth result = BandwidthArray.of(
                Operator.OR, Bandwidths.UNLIMITED, Bandwidths.UNLIMITED);
        assertThat(result).isSameAs(Bandwidths.UNLIMITED);
    }

    @Test
    void givenMultipleBandwidthsMixed_shouldReturnInstanceHavingOnlyLimitedElements() {
        final Operator operator = Operator.OR;
        final Bandwidth two = Bandwidths.ofSeconds(2);
        final Bandwidth one = Bandwidths.ofSeconds(1);
        final Bandwidth expected = BandwidthArray.of(operator, two, one);
        final Bandwidth result = BandwidthArray.of(
                operator, Bandwidths.UNLIMITED, two, Bandwidths.UNLIMITED, one);
        assertThat(expected).isEqualTo(result);
    }

    @Test
    void givenOneUnlimitedAndOneLimitedBandwidth_shouldReturnSingleUnlimited() {
        final Bandwidth expected = Bandwidths.ofSeconds(7);
        final Bandwidth result = BandwidthArray.of(
                Operator.AND, Bandwidths.UNLIMITED, expected);
        assertThat(result).isSameAs(expected);
    }

    @Test
    void givenOperatorNone_shouldThrowException() {
        final Bandwidth one = Bandwidths.ofSeconds(1);
        final Bandwidth two = Bandwidths.ofSeconds(2);
        assertThatThrownBy(() -> BandwidthArray.of(Operator.NONE, one, two))
                .isInstanceOf(RuntimeException.class);
    }
}