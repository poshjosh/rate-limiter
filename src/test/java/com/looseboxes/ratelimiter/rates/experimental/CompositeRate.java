package com.looseboxes.ratelimiter.rates.experimental;


import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.util.Experimental;

/**
 * A Rate composed of multiple Rates.
 *
 * Any operation on this object will affect all the composite members.
 */
@Experimental
public interface CompositeRate extends Rate {
    Rate[] getRates();
}
