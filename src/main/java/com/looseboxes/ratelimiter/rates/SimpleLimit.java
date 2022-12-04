package com.looseboxes.ratelimiter.rates;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.util.*;

final class SimpleLimit implements Limit, Serializable {

    private static final Logger LOG = LoggerFactory.getLogger(SimpleLimit.class);

    private static final long serialVersionUID = 9081726354000000002L;

    private final Logic logic;

    private final Rate[] rates;

    SimpleLimit(Rate[] rates, Logic logic) {
        this.logic = Objects.requireNonNull(logic);
        this.rates = new Rate[rates.length];
        System.arraycopy(rates, 0, this.rates, 0, rates.length);
    }

    @Override
    public int compareTo(Rate other) {

        int resetCount = 0;
        int failCount = 0;

        for(Rate rate : rates) {

            final int comparison = other.compareTo(rate);

            if(comparison == 0) {

                ++resetCount;

            }else if(comparison > 0) {

                ++failCount;
            }
        }

        if(LOG.isTraceEnabled()) {
            LOG.trace("Limit exceeded: {}, by: {}, limit: {}", isExceeded(failCount), other, this);
        }

        if (isExceeded(resetCount)) {
            return 0;
        }

        return isExceeded(failCount) ? 1 : -1;
    }

    @Override
    public Logic getLogic() {
        return this.logic;
    }

    @Override
    public Rate[] getRates() {
        return this.rates;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        SimpleLimit that = (SimpleLimit) o;
        return logic == that.logic && Arrays.equals(rates, that.rates);
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(logic);
        result = 31 * result + Arrays.hashCode(rates);
        return result;
    }

    @Override
    public String toString() {
        return "SimpleLimit{" + "logic=" + logic + ", rates=" + Arrays.toString(rates) + '}';
    }
}
