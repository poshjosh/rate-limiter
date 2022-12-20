package com.looseboxes.ratelimiter.util;

import com.looseboxes.ratelimiter.Rate;

import java.util.Arrays;
import java.util.stream.Stream;

public interface CompositeRate {

    CompositeRate EMPTY_OR = CompositeRate.of(Operator.OR);
    CompositeRate EMPTY_AND = CompositeRate.of(Operator.AND);

    static CompositeRate empty(Operator operator) {
        return Operator.AND.equals(operator) ? EMPTY_AND : EMPTY_OR;
    }

    static CompositeRate of(Rate... rates) {
        return of(Operator.OR, rates);
    }

    static CompositeRate of(Operator operator, Rate... rates) {
        return new SimpleCompositeRate(rates, operator);
    }

    default boolean hasRates() { return numberOfRates() > 0; }

    default int numberOfRates() {
        return getRates().length;
    }

    default Stream<Rate> stream() {
        return Arrays.stream(getRates());
    }

    Operator getOperator();
    Rate[] getRates();

    /**
     * Compare this CompositeRate to a {@link Rate}.
     *
     * @param rate The Rate to compare this CompositeRate to
     * @return
     * <p><b>The return value represents the following:</b></p>
     * <ul>
     *     <li>+1 = HAS EXCEEDED LIMIT</li>
     *     <li>0 = IS AT A THRESHOLD (Should be reset)</li>
     *     <li>-1 = IS WITHIN LIMIT</li>
     * </ul>
     * @see Rate#compareTo(Rate)
     */
    default int compareTo(Rate rate) {

        int resetCount = 0;
        int failCount = 0;

        for(Rate curr : getRates()) {

            final int comparison = rate.compareTo(curr);

            if(comparison == 0) {

                ++resetCount;

            }else if(comparison > 0) {

                ++failCount;
            }
        }

        final boolean limitExceeded = isExceeded(failCount);

        if (isExceeded(resetCount)) {
            return 0;
        }

        return limitExceeded ? 1 : -1;
    }

    default boolean isExceeded(int numberOfHits) {
        Operator operator = getOperator();
        return (Operator.OR.equals(operator) && numberOfHits > 0)
                || (Operator.AND.equals(operator) && numberOfHits >= numberOfRates());
    }
}
