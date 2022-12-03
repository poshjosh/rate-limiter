package com.looseboxes.ratelimiter.rates;

import java.io.Serializable;

/**
 * Represents a rate.
 *
 * A rate is a ratio between 2 related quantities, each with their own unit of measurement.
 * This interface represents a basic rate contract, which is defines, how a rate is:
 * <ul>
 *     <li>Compared to another rate (of the same type)</li>
 *     <li>Incremented</li>
 * </ul>
 * Some examples of rate related quantities:
 * Distance and time
 * Count and time
 * Memory and time
 */
public interface Rate extends Comparable<Rate>, Serializable {

    Rate NONE = new Rate() {
        @Override
        public Rate increment(int amount) {
            return this;
        }
        @Override
        public int compareTo(Rate other) {
            return 0;
        }
        @Override
        public String toString() {
            return Rate.class.getName() + "$NONE";
        }
    };

    static Rate of(long amount, long duration) {
        return new AmountPerDuration(amount, duration, System.currentTimeMillis());
    }

    default <T extends Rate> T increment() {
        return increment(1);
    }

    <T extends Rate> T increment(int amount);

    /**
     * Compare this to another.
     *
     * @param other The Rate to compare this Rate to
     * @return
     * <p><b>The return value represents the following:</b></p>
     * <ul>
     *     <li>POSITIVE_INTEGER = HAS EXCEEDED LIMIT</li>
     *     <li>ZERO = IS AT A THRESHOLD (Should be reset)</li>
     *     <li>NEGATIVE_INTEGER = IS WITHIN LIMIT</li>
     * </ul>
     * @see Comparable#compareTo(Object)
     */
    @Override
    int compareTo(Rate other);
}
