package com.looseboxes.ratelimiter.rates;

import java.io.Serializable;

/**
 * Represents a rate.
 *
 * A rate is a ratio between 2 related quantities, each with their own unit of measurement.
 * This interface represents a basic rate contract, which is only defines how it is compared
 * to other rates (usually of the same kind), as well as how it is incremented.
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

    default Rate increment() {
        return increment(1);
    }

    Rate increment(int amount);

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
