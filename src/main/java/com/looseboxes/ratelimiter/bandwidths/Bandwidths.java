package com.looseboxes.ratelimiter.bandwidths;

import com.looseboxes.ratelimiter.util.Experimental;
import com.looseboxes.ratelimiter.util.Operator;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Stream;

import static java.util.concurrent.TimeUnit.SECONDS;

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

    public static Bandwidths copyOf(Bandwidths bandwidths) {
        final Bandwidth [] members = bandwidths.getMembers();
        final Bandwidth [] copy = new Bandwidth[members.length];
        for (int i = 0; i < copy.length; i++) {
            copy[i] = members[i].copy();
        }
        return Bandwidths.of(bandwidths.getOperator(), copy);
    }

    public static Bandwidths copyOf(Bandwidths bandwidths, long nowMicros) {
        final Bandwidth [] members = bandwidths.getMembers();
        final Bandwidth [] copy = new Bandwidth[members.length];
        for (int i = 0; i < copy.length; i++) {
            copy[i] = members[i].copy(nowMicros);
        }
        return Bandwidths.of(bandwidths.getOperator(), copy);
    }

    public static Bandwidths copyOf(Bandwidths bandwidths, long nowMicros, double... permitsPerSecond) {
        final Bandwidth [] members = bandwidths.getMembers();
        final Bandwidth [] copy = new Bandwidth[members.length];
        for (int i = 0; i < copy.length; i++) {
            copy[i] = members[i].copy(permitsPerSecond[i], nowMicros);
        }
        return Bandwidths.of(bandwidths.getOperator(), copy);
    }

    private static final long serialVersionUID = 9081726354000000020L;

    private final Operator operator;

    private final Bandwidth[] members;

    private Bandwidths(Operator operator, Bandwidth... members) {
        this.operator = Objects.requireNonNull(operator);
        this.members = Arrays.copyOf(members, members.length);
    }


    public boolean hasMembers() { return memberCount() > 0; }

    public int memberCount() {
        return getMembers().length;
    }

    public Stream<Bandwidth> stream() {
        return Arrays.stream(getMembers());
    }

    public boolean isExceeded(int failureCount) {
        Operator operator = getOperator();
        return (Operator.OR.equals(operator) && failureCount > 0)
                || (Operator.AND.equals(operator) && failureCount >= memberCount());
    }

    /**
     * The interval between two unit requests, at our stable rate. E.g., a stable rate of 5 permits
     * per second has a stable interval of 200ms.
     */
    @Experimental
    public long getStableIntervalMicros() {
        final double permitsPerSecond = getPermitsPerSecond();
        return (long)(SECONDS.toMicros(1L) / permitsPerSecond);
    }

    /**
     * Returns the stable rate (as {@code permits per seconds}) with which an eligible {@code Bandwidth} in this
     * {@code Bandwidths} is configured with. The initial value is the same as the {@code permitsPerSecond}
     * argument passed in the factory method that produced the {@code Bandwidth}.
     * @see #getAllPermitsPerSecond()
     */
    @Experimental
    public double getPermitsPerSecond() {
        final boolean isAnd = Operator.AND.equals(operator);
        double [] arr = getAllPermitsPerSecond();
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
    private double [] getAllPermitsPerSecond() {
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
        return "SimpleBandwidths{" + "operator=" + operator + " " + Arrays.toString(members) + "}";
    }

    private static class SecureSerializationProxy implements Serializable {

        private static final long serialVersionUID = 9081726354000000021L;

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
