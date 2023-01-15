package io.github.poshjosh.ratelimiter.util;

import io.github.poshjosh.ratelimiter.bandwidths.Bandwidths;

/**
 * A logical operator for a group of bandwidths/rates.
 * @see Bandwidths
 */
public enum Operator {

    /**
     * Fail when all limits fail
     */
    AND,

    /**
     * Fail when any limit fails.
     */
    OR,

    DEFAULT
}
