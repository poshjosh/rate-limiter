package com.looseboxes.ratelimiter.rates;

public interface Rate extends Comparable<Rate>, Cloneable {

    Rate NONE = new Rate() {
        @Override
        public Rate clone() {
            return this;
        }
        @Override
        public Rate increment() {
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

    Rate clone();

    Rate increment();

    /**
     * Compare this to another.
     *
     * <p><b>The return value represents the following:</b></p>
     *
     * <ul>
     *     <li>POSITIVE_INTEGER = HAS EXCEEDED LIMIT</li>
     *     <li>ZERO = IS AT A THRESHOLD (Should be reset)</li>
     *     <li>NEGATIVE_INTEGER = IS WITHIN LIMIT</li>
     * </ul>
     *
     * @param other
     * @return
     * @see Comparable#compareTo(Object)
     */
    @Override
    int compareTo(Rate other);
}
