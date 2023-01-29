package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.annotations.Beta;
import io.github.poshjosh.ratelimiter.Operator;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Objects;

public final class Bandwidths implements Bandwidth, Serializable{

    private static final Operator DEFAULT_OPERATOR = Operator.OR;
    public static final Bandwidths EMPTY_OR = Bandwidths.of(Operator.OR);
    public static final Bandwidths EMPTY_AND = Bandwidths.of(Operator.AND);

    public static Bandwidths empty(Operator operator) {
        switch(operator) {
            case AND: return EMPTY_AND;
            case OR:
            case DEFAULT: return EMPTY_OR;
            default: throw new IllegalArgumentException("Unexpected operator: " + operator);
        }
    }

    public static Bandwidths and(Bandwidth... bandwidths) {
        return of(Operator.AND, bandwidths);
    }

    public static Bandwidths or(Bandwidth... bandwidths) {
        return of(Operator.OR, bandwidths);
    }

    public static Bandwidths of(Bandwidth... bandwidths) {
        return of(Operator.DEFAULT, bandwidths);
    }

    public static Bandwidths of(Bandwidths bandwidths) {
        return new Bandwidths(bandwidths);
    }

    public static Bandwidths of(Operator operator, Bandwidth... bandwidths) {
        return new Bandwidths(operator, bandwidths);
    }

    private static final long serialVersionUID = 20L;

    private final Operator operator;

    private final Bandwidth[] bandwidths;

    private Bandwidths(Bandwidths bandwidths) {
        this(bandwidths.operator, bandwidths.bandwidths);
    }

    private Bandwidths(Operator operator, Bandwidth... bandwidths) {
        this.operator = Operator.DEFAULT.equals(operator) ? DEFAULT_OPERATOR : operator;
        this.bandwidths = Arrays.copyOf(bandwidths, bandwidths.length);
    }

    public boolean hasBandwidths() {
        return this.bandwidths.length > 0;
    }

    @Override
    public Bandwidths with(long nowMicros) {
        final Bandwidth [] copies = new Bandwidth[bandwidths.length];
        for (int i = 0; i < copies.length; i++) {
            copies[i] = bandwidths[i].with(nowMicros);
        }
        return Bandwidths.of(operator, copies);
    }

    @Override
    public long queryEarliestAvailable(long nowMicros) {
        final boolean AND = Operator.AND.equals(operator);
        long result = -1;
        for(Bandwidth bandwidth : bandwidths) {
            final long current = bandwidth.queryEarliestAvailable(nowMicros);
            if (result == -1) {
                result = current;
            }
            if (AND && current < result) {
                result = current;
                continue;
            }
            if (!AND && current > result) {
                result = current;
            }
        }
        return result;
    }

    @Override
    public long reserveEarliestAvailable(int permits, long nowMicros) {
        final boolean AND = Operator.AND.equals(operator);
        long result = -1;
        for(Bandwidth bandwidth : bandwidths) {
            final long current = bandwidth.reserveEarliestAvailable(permits, nowMicros);
            if (result == -1) {
                result = current;
            }
            if (AND && current < result) {
                result = current;
                continue;
            }
            if (!AND && current > result) {
                result = current;
            }
        }
        return result;
    }

    /**
     * Returns the stable rate (as {@code permits per seconds}) with which an eligible {@code Bandwidth} in this
     * {@code Bandwidths} is configured with. The initial value is the same as the {@code permitsPerSecond}
     * argument passed in the factory method that produced the {@code Bandwidth}.
     * @see #getAllPermitsPerSecond()
     */
    @Beta
    public double getPermitsPerSecond() {
        final boolean AND = Operator.AND.equals(operator);
        double [] arr = getAllPermitsPerSecond();
        double result = -1;
        for(double e : arr) {
            // TODO Why are we returning the max here for AND, as oppose to min
            //  returned for AND in other methods?
            result = result == - 1 ? e : (AND ? Math.max(e, result) : Math.min(e, result));
        }
        return result;
    }

    /**
     * Returns the stable rates (as {@code permits per seconds}) with which each {@code Bandwidth} in this
     * {@code Bandwidths} is configured with. The initial value of each, is the same as the {@code permitsPerSecond}
     * argument passed in the factory method that produced each {@code Bandwidth}.
     */
    private double [] getAllPermitsPerSecond() {
        final double [] permitsPerSecond = new double[bandwidths.length];
        for(int i = 0; i < bandwidths.length; i++) {
            permitsPerSecond[i] = bandwidths[i].getPermitsPerSecond();
        }
        return permitsPerSecond;
    }

    public Operator getOperator() {
        return this.operator;
    }

    /**
     * @return A copy of the member bandwidths
     */
    public Bandwidth[] getBandwidths() {
        return Arrays.copyOf(bandwidths, bandwidths.length);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        Bandwidths that = (Bandwidths) o;
        return operator == that.operator && Arrays.equals(bandwidths, that.bandwidths);
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(operator);
        result = 31 * result + Arrays.hashCode(bandwidths);
        return result;
    }

    @Override
    public String toString() {
        return "Bandwidths{" + "operator=" + operator + " " + Arrays.toString(bandwidths) + "}";
    }

    private static class SecureSerializationProxy implements Serializable {

        private static final long serialVersionUID = 21L;

        private final Operator operator;
        private final Bandwidth[] members;


        public SecureSerializationProxy(Bandwidths candidate){
            this.operator = candidate.operator;
            this.members = candidate.bandwidths;
        }
        private Object readResolve() throws InvalidObjectException {
            return new Bandwidths(operator, members);
        }
    }

    private Object writeReplace(){
        return new Bandwidths.SecureSerializationProxy(this);
    }
    private void readObject(ObjectInputStream ois) throws InvalidObjectException {
        throw new InvalidObjectException("Secure proxy must be used for serialization");
    }
}
