package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.annotations.Beta;
import io.github.poshjosh.ratelimiter.Operator;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.util.*;

public final class Bandwidths implements Bandwidth, Serializable{

    private static final Operator DEFAULT_OPERATOR = Operator.OR;
    public static final Bandwidth EMPTY_OR = Bandwidths.of(Operator.OR);
    public static final Bandwidth EMPTY_AND = Bandwidths.of(Operator.AND);

    public static Bandwidth empty(Operator operator) {
        switch(operator) {
            case AND: return EMPTY_AND;
            case OR:
            case NONE: return EMPTY_OR;
            default: throw new IllegalArgumentException("Unexpected operator: " + operator);
        }
    }

    public static Bandwidth and(Bandwidth... bandwidths) {
        return of(Operator.AND, bandwidths);
    }

    public static Bandwidth or(Bandwidth... bandwidths) {
        return of(Operator.OR, bandwidths);
    }

    public static Bandwidth of(Bandwidth... bandwidths) {
        return of(Bandwidths.DEFAULT_OPERATOR, bandwidths);
    }

    public static Bandwidth of(Bandwidths bandwidths) {
        return new Bandwidths(bandwidths);
    }

    public static Bandwidth of(Operator operator, Bandwidth... bandwidths) {
        return of(buildId(operator, bandwidths), operator, bandwidths);
    }

    public static Bandwidth of(String id, Operator operator, Bandwidth... bandwidths) {
        return new Bandwidths(id, operator, bandwidths);
    }

    private static String buildId(Operator operator, Bandwidth...bandwidths) {
        Objects.requireNonNull(operator);
        StringBuilder b = new StringBuilder(64 * bandwidths.length);
        b.append(operator);
        if (bandwidths != null) {
            for(Bandwidth bandwidth : bandwidths) {
                b.append('{').append(Double.toHexString(bandwidth.getPermitsPerSecond())).append('-')
                        .append(Long.toHexString(bandwidth.queryEarliestAvailable(0)))
                        .append('}');
            }
        }
        // Best effort only
        return UUID.nameUUIDFromBytes(b.toString().getBytes(StandardCharsets.UTF_8)).toString();
    }

    private static final long serialVersionUID = 20L;

    private final String id;

    private final Operator operator;

    private final Bandwidth[] bandwidths;

    private Bandwidths(Bandwidths bandwidths) {
        this(bandwidths.id, bandwidths.operator, bandwidths.bandwidths);
    }

    private Bandwidths(String id, Operator operator, Bandwidth... bandwidths) {
        this.id = Objects.requireNonNull(id);
        this.operator = Objects.requireNonNull(operator);
        this.bandwidths = Arrays.copyOf(bandwidths, bandwidths.length);
        if (Operator.NONE.equals(operator)) {
            throw Checks.notSupported(this, "operator " + operator);
        }
    }

    @Override
    public Bandwidth with(long nowMicros) {
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

    public boolean hasBandwidths() {
        return bandwidths.length != 0;
    }

    public String getId() {
        return id;
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

    @Override public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        Bandwidths that = (Bandwidths) o;
        return id.equals(that.id);
    }

    @Override public int hashCode() {
        return Objects.hash(id);
    }

    @Override
    public String toString() {
        return "Bandwidths{id=" + id + ", " + operator + Arrays.toString(bandwidths) + "}";
    }

    private static class SecureSerializationProxy implements Serializable {

        private static final long serialVersionUID = 21L;

        private final String id;
        private final Operator operator;
        private final Bandwidth[] members;

        public SecureSerializationProxy(Bandwidths candidate){
            this.id = candidate.id;
            this.operator = candidate.operator;
            this.members = candidate.bandwidths;
        }
        private Object readResolve() throws InvalidObjectException {
            return new Bandwidths(id, operator, members);
        }
    }

    private Object writeReplace(){
        return new Bandwidths.SecureSerializationProxy(this);
    }
    private void readObject(ObjectInputStream ois) throws InvalidObjectException {
        throw new InvalidObjectException("Secure proxy must be used for serialization");
    }
}
