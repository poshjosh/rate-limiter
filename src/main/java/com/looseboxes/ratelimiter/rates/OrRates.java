package com.looseboxes.ratelimiter.rates;

import java.util.Arrays;

/**
 * A Rate implementation representing multiple Rates composed by logical OR
 *
 * Any operation on this object will affect all the composite members.
 */
public class OrRates implements CompositeRate{

    private final Rate [] rates;

    public OrRates(Rate... rates) {
        this.rates = Arrays.copyOf(rates, rates.length);
    }

    @Override
    public Rate clone() {
        return new OrRates(rates);
    }

    @Override
    public Rate increment() {
        final Rate [] updates = new Rate[rates.length];
        for(int i = 0; i< rates.length; i++) {
            updates[i] = rates[i].increment();
        }
        return new OrRates(updates);
    }

    /**
     * Compare all the member Rates to the specified Rate argument
     * @param other The other to compare all the member rates to
     * @return A number greater than zero if at least one comparison is greater than zero, a number equal to zero if
     * all comparisons are equal to zero, otherwise a number less than zero.
     */
    @Override
    public int compareTo(Rate other) {
        return other instanceof CompositeRate ? compareToAll(((CompositeRate)other).getRates()) : compareToOne(other);
    }

    private int compareToAll(Rate [] others) {
        int neutralCount = 0;
        for(int i=0; i<others.length; i++) {
            final int n = rates[i].compareTo(others[i]);
            if(n == 0) {
                ++neutralCount;
            }else if(n > 0) {
                return n;
            }
        }
        return neutralCount == rates.length ? 0 : - rates.length;
    }

    private int compareToOne(Rate other) {
        int neutralCount = 0;
        for(Rate rate : rates) {
            final int n = rate.compareTo(other);
            if(n == 0) {
                ++neutralCount;
            }else if(n > 0) {
                return n;
            }
        }
        return neutralCount == rates.length ? 0 : - rates.length;
    }

    @Override
    public Rate[] getRates() {
        return rates;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        OrRates orRates = (OrRates) o;
        return Arrays.equals(rates, orRates.rates);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(rates);
    }

    @Override
    public String toString() {
        return "OrRates{" +
                ", rates=" + Arrays.toString(rates) +
                '}';
    }
}
