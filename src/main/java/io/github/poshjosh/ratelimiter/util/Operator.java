package io.github.poshjosh.ratelimiter.util;

import io.github.poshjosh.ratelimiter.bandwidths.Bandwidths;

/**
 * A group of bandwidths/rates are connected by an operator.
 * @see {@link Bandwidths}
 */
public enum Operator {AND, OR}
