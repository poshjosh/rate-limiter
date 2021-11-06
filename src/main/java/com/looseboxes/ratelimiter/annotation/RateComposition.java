package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.CompositeRate;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.rates.Rates;

import java.util.Arrays;
import java.util.Objects;

/**
 * Represents rates and an ID for an annotated element
 * @param <ID> The type of the id
 */
public class RateComposition<ID>{

    private ID id;
    private Rate[] rates;
    private Rates.Logic logic = Rates.Logic.OR;

    public RateComposition() { }

    public RateComposition(ID id) {
        this.id = id;
    }

    public ID getId() {
        return id;
    }

    public void setId(ID id) {
        this.id = id;
    }

    public RateComposition<ID> id(ID id) {
        this.id = id;
        return this;
    }

    public Rate[] getRates() {
        return rates;
    }

    public void setRates(Rate[] rates) {
        this.rates = rates;
    }

    public RateComposition<ID> rates(Rate [] rates) {
        this.rates = rates;
        return this;
    }

    public Rates.Logic getLogic() {
        return logic;
    }

    public void setLogic(Rates.Logic logic) {
        this.logic = logic;
    }

    public RateComposition<ID> logic(Rates.Logic logic) {
        this.logic = logic;
        return this;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        RateComposition<?> that = (RateComposition<?>) o;
        return id.equals(that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

    @Override
    public String toString() {
        return "RateComposition{" +
                "id=" + id +
                ", logic=" + logic +
                ", rates=" + Arrays.toString(rates) +
                '}';
    }
}