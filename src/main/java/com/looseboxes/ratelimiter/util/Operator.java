package com.looseboxes.ratelimiter.util;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;

/**
 * A group of bandwidths/rates are connected by an operator.
 * @see {@link Bandwidths}
 */
public enum Operator {AND, OR}
