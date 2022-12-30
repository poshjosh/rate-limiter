package com.looseboxes.ratelimiter.bandwidths;

import com.looseboxes.ratelimiter.annotations.Beta;
import com.looseboxes.ratelimiter.util.Operator;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Stream;

public final class Bandwidths implements Bandwidth, Serializable{

    public static final Bandwidths EMPTY_OR = Bandwidths.of(Operator.OR);
    public static final Bandwidths EMPTY_AND = Bandwidths.of(Operator.AND);

    public static Bandwidths empty(Operator operator) {
        return Operator.AND.equals(operator) ? EMPTY_AND : EMPTY_OR;
    }

    public static Bandwidths and(Bandwidth... bandwidths) {
        return of(Operator.AND, bandwidths);
    }

    public static Bandwidths or(Bandwidth... bandwidths) {
        return of(Operator.OR, bandwidths);
    }

    public static Bandwidths of(Bandwidth... bandwidths) {
        return of(Operator.OR, bandwidths);
    }

    public static Bandwidths of(Bandwidths bandwidths) {
        return new Bandwidths(bandwidths);
    }

    public static Bandwidths of(Operator operator, Bandwidth... bandwidths) {
        return new Bandwidths(operator, bandwidths);
    }

    private static final long serialVersionUID = 20L;

    private final Operator operator;

    private final Bandwidth[] members;

    private Bandwidths(Bandwidths bandwidths) {
        this(bandwidths.operator, bandwidths.members);
    }

    private Bandwidths(Operator operator, Bandwidth... members) {
        this.operator = Objects.requireNonNull(operator);
        this.members = Arrays.copyOf(members, members.length);
    }

    public boolean isLimitExceeded(int failureCount) {
        return (Operator.OR.equals(operator) && failureCount > 0)
                || (Operator.AND.equals(operator) && failureCount >= members.length);
    }

    @Override
    public Bandwidths with(long nowMicros) {
        final Bandwidth [] copies = new Bandwidth[members.length];
        for (int i = 0; i < copies.length; i++) {
            copies[i] = members[i].with(nowMicros);
        }
        return Bandwidths.of(operator, copies);
    }

    @Override
    public long queryEarliestAvailable(long nowMicros) {
        final boolean AND = Operator.AND.equals(operator);
        long result = -1;
        for(Bandwidth bandwidth : members) {
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
        for(Bandwidth bandwidth : members) {
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

    public Stream<Bandwidth> stream() {
        return Arrays.stream(getMembers());
    }

    /**
     * Returns the stable rate (as {@code permits per seconds}) with which an eligible {@code Bandwidth} in this
     * {@code Bandwidths} is configured with. The initial value is the same as the {@code permitsPerSecond}
     * argument passed in the factory method that produced the {@code Bandwidth}.
     * @see #getAllRates()
     */
    @Beta
    public double getPermitsPerSecond() {
        final boolean AND = Operator.AND.equals(operator);
        double [] arr = getAllRates();
        double result = -1;
        for(double e : arr) {
            // TODO Why are we returning the max here for AND, as oppose to min in other methods?
            result = result == - 1 ? e : (AND ? Math.max(e, result) : Math.min(e, result));
        }
        return result;
    }

    /**
     * Returns the stable rates (as {@code permits per seconds}) with which each {@code Bandwidth} in this
     * {@code Bandwidths} is configured with. The initial value of each, is the same as the {@code permitsPerSecond}
     * argument passed in the factory method that produced each {@code Bandwidth}.
     */
    private double [] getAllRates() {
        final double [] permitsPerSecond = new double[members.length];
        for(int i = 0; i < members.length; i++) {
            permitsPerSecond[i] = members[i].getPermitsPerSecond();
        }
        return permitsPerSecond;
    }

    public Operator getOperator() {
        return this.operator;
    }

    public Bandwidth[] getMembers() {
        return this.members;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        Bandwidths that = (Bandwidths) o;
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
        return "Bandwidths{" + "operator=" + operator + " " + Arrays.toString(members) + "}";
    }

    private static class SecureSerializationProxy implements Serializable {

        private static final long serialVersionUID = 21L;

        private final Operator operator;
        private final Bandwidth[] members;


        public SecureSerializationProxy(Bandwidths candidate){
            this.operator = candidate.operator;
            this.members = candidate.members;
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
