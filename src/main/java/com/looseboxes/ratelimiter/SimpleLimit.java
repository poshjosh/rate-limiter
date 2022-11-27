package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Logic;
import com.looseboxes.ratelimiter.rates.Rate;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Objects;

public class SimpleLimit implements Limit, Serializable {

    private static final long serialVersionUID = 9081726354000000002L;

    private final Logic logic;

    private final Rate[] rates;

    public SimpleLimit(Rate[] rates, Logic logic) {
        this.logic = Objects.requireNonNull(logic);
        this.rates = new Rate[rates.length];
        System.arraycopy(rates, 0, this.rates, 0, rates.length);
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
