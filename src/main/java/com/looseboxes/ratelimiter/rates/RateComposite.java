package com.looseboxes.ratelimiter.rates;

import java.util.Arrays;
import java.util.Objects;

/**
 * A Rate implementation composed of multiple Rates.
 *
 * Any operation on this object will affect all the composite members.
 */
public class RateComposite implements Rate{

    public enum Logic{AND, OR};

    private final Logic logic;

    private final Rate [] delegates;

    public RateComposite(Logic logic, Rate... delegates) {
        this.logic = Objects.requireNonNull(logic);
        this.delegates = Arrays.copyOf(delegates, delegates.length);
    }

    @Override
    public Rate clone() {
        return new RateComposite(logic, delegates);
    }

    @Override
    public Rate increment() {
        final Rate [] updates = new Rate[delegates.length];
        for(int i=0; i<delegates.length; i++) {
            updates[i] = delegates[i].increment();
        }
        return new RateComposite(logic, updates);
    }

    @Override
    public int compareTo(Rate other) {
        switch (logic) {
            case OR:
                return other instanceof RateComposite ? compareToBasedOnOr(((RateComposite)other).delegates) : compareToBasedOnOr(other);
            case AND:
                return other instanceof RateComposite ? compareToBasedOnAnd(((RateComposite)other).delegates) : compareToBasedOnAnd(other);
            default:
                throw new IllegalArgumentException("Unexpected logic: " + logic +
                        ", valid values: " + Arrays.toString(Logic.values()));
        }
    }

    private int compareToBasedOnOr(Rate [] others) {
        int neutralCount = 0;
        int result = 0;
        for(int i=0; i<others.length; i++) {
            final int n = delegates[i].compareTo(others[i]);
            if(n == 0) {
                ++neutralCount;
            }else if(n > 0) {
                result = n;
                break;
            }
        }
        return result != 0 ? result : neutralCount == delegates.length ? 0 : -1;
    }

    /**
     * Compare all the delegate Rates to the specified Rate argument
     * @param other The other to compare all the delegate rates to
     * @return A number greater than zero if at least one comparison is greater than zero.
     */
    private int compareToBasedOnOr(Rate other) {
        int neutralCount = 0;
        int result = 0;
        for(Rate delegate : delegates) {
            final int n = delegate.compareTo(other);
            if(n == 0) {
                ++neutralCount;
            }else if(n > 0) {
                result = n;
                break;
            }
        }
        return result != 0 ? result : neutralCount == delegates.length ? 0 : -1;
    }

    private int compareToBasedOnAnd(Rate [] others) {
        int neutralCount = 0;
        int result = 0;
        for(int i=0; i<others.length; i++) {
            final int n = delegates[i].compareTo(others[i]);
            if(n == 0) {
                ++neutralCount;
            }else if(n < 0) {
                result = n;
                break;
            }
        }
        return result != 0 ? result : neutralCount == delegates.length ? 0 : 1;
    }

    /**
     * Compare all the delegate Rates to the specified Rate argument
     * @param other The other to compare all the delegate rates to
     * @return A number greater than zero only if all comparisons are greater than zero.
     */
    private int compareToBasedOnAnd(Rate other) {
        int neutralCount = 0;
        int result = 0;
        for(Rate delegate : delegates) {
            final int n = delegate.compareTo(other);
            if(n == 0) {
                ++neutralCount;
            }else if(n < 0) {
                result = n;
                break;
            }
        }
        return result != 0 ? result : neutralCount == delegates.length ? 0 : 1;
    }

    @Override
    public String toString() {
        return "RateComposite{" +
                "logic=" + logic +
                ", delegates=" + Arrays.toString(delegates) +
                '}';
    }
}
