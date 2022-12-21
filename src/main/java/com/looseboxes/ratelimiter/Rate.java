package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.annotation.RateLimitProcessor;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.Objects;

/**
 * Represents a rate.
 *
 * A rate is a ratio between 2 related quantities, each with their own unit of measurement.
 * This interface represents a basic rate contract, which is defines:
 * <ul>
 *     <li>How a rate is compared to another rate (of the same type)</li>
 *     <li>How a rate is incremented</li>
 *     <li>The value of the rate (i.e. the ratio between the 2 quantities).</li>
 * </ul>
 * Some examples of rate related quantities:
 * Distance and time
 * Count and time
 * Memory and time
 */
public final class Rate implements Comparable<Rate>, Serializable {

    public static final Rate NONE = new Rate(0, 0);

    public static Rate of(long amount, long durationMillis) {
        return new Rate(amount, durationMillis);
    }

    private static final long serialVersionUID = 9081726354000000010L;

    private final long amount;
    private final long durationMillis;
    private final long timeCreatedMillis; // Not included in equals/hashCode

    private Rate(long amount, long durationMillis) {
        this(amount, durationMillis, System.currentTimeMillis());
    }

    private Rate(long amount, long durationMillis, long timeCreatedMillis) {
        final String limitError = RateLimitProcessor.getErrorMessageIfInvalidLimit(amount, null);
        if(limitError != null) {
            throw new IllegalArgumentException(limitError);
        }
        final String durationError = RateLimitProcessor.getErrorMessageIfInvalidDuration(durationMillis, null);
        if(durationError != null) {
            throw new IllegalArgumentException(durationError);
        }
        this.amount = amount;
        this.durationMillis = durationMillis;
        this.timeCreatedMillis = timeCreatedMillis;
    }

    @Override
    public int compareTo(Rate other) {
        if(amount == other.amount && durationMillis == other.durationMillis) {
            return 0;
        }
        if (amount > other.amount) {
            //System.out.println("Result: " + result + ", lhs: " + this + ", rhs: " + other);
            return durationMillis > other.durationMillis ? 0 : 1;
        }
        //System.out.println("Result: -1, lhs: " + this + ", rhs: " + other);
        return -1;
    }

    public double getRateMillis() {
        return (double)amount / durationMillis;
    }

    public long getAmount() {
        return amount;
    }

    public Duration getDuration() {
        return Duration.of(getDurationMillis(), ChronoUnit.MILLIS);
    }

    public long getDurationMillis() {
        return durationMillis;
    }

    public long getTimeCreatedMillis() {
        return timeCreatedMillis;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Rate that = (Rate) o;
        return amount == that.amount && durationMillis == that.durationMillis;
    }

    @Override
    public int hashCode() {
        return Objects.hash(amount, durationMillis);
    }

    @Override
    public String toString() {
        return "Rate@" + Integer.toHexString(hashCode()) +
                "{amount=" + amount + ", duration=" + durationMillis + "millis}";
    }

    private static class SerializationProxy implements Serializable{

        private static final long serialVersionUID = 9081726354000000011L;

        private final long amount;
        private final long durationMillis;
        private final long timeCreatedMillis;

        public SerializationProxy(Rate candidate){
            this.amount = candidate.amount;
            this.durationMillis = candidate.durationMillis;
            this.timeCreatedMillis = candidate.timeCreatedMillis;
        }
        private Object readResolve() throws InvalidObjectException {
            return new Rate(amount, durationMillis, timeCreatedMillis);
        }
    }

    private Object writeReplace(){ return new Rate.SerializationProxy(this); }
    private void readObject(ObjectInputStream ois) throws InvalidObjectException {
        throw new InvalidObjectException("Secure proxy must be used for serialization");
    }
}
