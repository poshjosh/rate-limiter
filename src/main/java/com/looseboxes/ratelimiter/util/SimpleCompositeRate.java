package com.looseboxes.ratelimiter.util;

import com.looseboxes.ratelimiter.Rate;

import java.io.Serializable;
import java.util.*;

final class SimpleCompositeRate implements CompositeRate, Serializable {

    private static final long serialVersionUID = 9081726354000000020L;

    private final Operator operator;

    private final Rate[] members;

    SimpleCompositeRate(Rate[] members, Operator operator) {
        this.operator = Objects.requireNonNull(operator);
        this.members = new Rate[members.length];
        System.arraycopy(members, 0, this.members, 0, members.length);
    }

    @Override
    public Operator getOperator() {
        return this.operator;
    }

    @Override
    public Rate[] getRates() {
        return this.members;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        SimpleCompositeRate that = (SimpleCompositeRate) o;
        return operator == that.operator && Arrays.equals(members, that.members);
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(operator);
        result = 31 * result + Arrays.hashCode(members);
        return result;
    }

    @Override
    public String toString() {
        return "SimpleCompositeRate{" + "operator=" + operator + ", rates=" + Arrays.toString(members) + '}';
    }
}
