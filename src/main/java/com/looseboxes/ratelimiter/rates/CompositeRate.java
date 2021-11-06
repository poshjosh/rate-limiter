package com.looseboxes.ratelimiter.rates;


/**
 * A Rate composed of multiple Rates.
 *
 * Any operation on this object will affect all the composite members.
 */
public interface CompositeRate extends Rate{
    Rate[] getRates();
}
