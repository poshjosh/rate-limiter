package com.looseboxes.ratelimiter.bandwidths;

import com.looseboxes.ratelimiter.annotations.Experimental;
import com.looseboxes.ratelimiter.util.Operator;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Stream;

public final class Bandwidths implements Serializable{

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

    public static Bandwidths of(Operator operator, Bandwidth... bandwidths) {
        return new Bandwidths(operator, bandwidths);
    }

    private static final long serialVersionUID = 20L;

    private final Operator operator;

    private final Bandwidth[] members;

    private Bandwidths(Operator operator, Bandwidth... members) {
        this.operator = Objects.requireNonNull(operator);
        this.members = Arrays.copyOf(members, members.length);
    }

    public boolean canAcquire(long nowMicros, long timeoutMicros) {
        int failureCount = 0;
        for (Bandwidth bandwidth : members) {
            if (!bandwidth.canAcquire(nowMicros, timeoutMicros)) {
                ++failureCount;
            }
        }
        return !isLimitExceeded(failureCount);
    }

    private boolean isLimitExceeded(int failureCount) {
        return (Operator.OR.equals(operator) && failureCount > 0)
                || (Operator.AND.equals(operator) && failureCount >= memberCount());
    }

    /**
     * Reserves next ticket and returns the wait time that the caller must wait for.
     *
     * @return the required wait time, never negative
     */
    public long reserveAndGetWaitLength(int permits, long nowMicros) {
        final boolean AND = Operator.AND.equals(operator);
        long waitTime = -1;
        for(Bandwidth bandwidth : members) {
            final long currWaitTime = bandwidth.reserveAndGetWaitLength(permits, nowMicros);
            if (waitTime == -1) {
                waitTime = currWaitTime;
            }
            if (AND && currWaitTime < waitTime) {
                waitTime = currWaitTime;
                continue;
            }
            if (!AND && currWaitTime > waitTime) {
                waitTime = currWaitTime;
            }
        }
        return waitTime;
    }

    public boolean hasMembers() { return memberCount() > 0; }

    public int memberCount() {
        return members.length;
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
    @Experimental
    public double getRate() {
        final boolean isAnd = Operator.AND.equals(operator);
        double [] arr = getAllRates();
        double result = -1;
        for(double e : arr) {
            result = result == - 1 ? e : (isAnd ? Math.max(e, result) : Math.min(e, result));
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
            permitsPerSecond[i] = members[i].getRate();
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
