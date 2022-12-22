package com.wip;

import com.looseboxes.ratelimiter.Checks;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Objects;

public final class AmountPerDuration implements Comparable<AmountPerDuration>, Serializable {

    private static final long serialVersionUID = 9081726354000000010L;

    private final long amount;
    private final long durationMillis;
    private final long timeCreatedMillis;

    AmountPerDuration(long amount, long durationMillis, long timeCreatedMillis) {
        Checks.requireFalse(amount < 0, "Limit must positive. Limit: " + amount);
        Checks.requireFalse(durationMillis < 0, "Duration must be positive. Duration: " + durationMillis + " millis");
        this.amount = amount;
        this.durationMillis = durationMillis;
        this.timeCreatedMillis = timeCreatedMillis;
    }

    public double getValue() {
        if (amount == 0) {
            return Double.MIN_VALUE;
        }
        return (double) amount / durationMillis;
    }

    public int compareTo(AmountPerDuration amountPerDuration) {
        if(amount == amountPerDuration.amount && durationMillis == amountPerDuration.durationMillis) {
            return 0;
        }
        if (amount > amountPerDuration.amount) {
            //System.out.println("Result: " + result + ", lhs: " + this + ", rhs: " + other);
            return durationMillis > amountPerDuration.durationMillis ? 0 : 1;
        }
        //System.out.println("Result: -1, lhs: " + this + ", rhs: " + other);
        return -1;
    }

    public AmountPerDuration increment(int amount) {
        if (amount < 0) {
            throw new IllegalArgumentException("Invalid amount" + amount);
        }
        // @TODO Do we pass in timeCreated or use System.currentTimeMillis() ?
        return new AmountPerDuration(incrementAmount(amount), incrementDuration(), this.timeCreatedMillis);
    }

    private long incrementAmount(int amount) {
        return this.amount + amount;
    }

    private long incrementDuration() {
        return durationMillis + (System.currentTimeMillis() - timeCreatedMillis);
    }

    public long getAmount() {
        return amount;
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
        AmountPerDuration that = (AmountPerDuration) o;
        return amount == that.amount && durationMillis == that.durationMillis;
    }

    @Override
    public int hashCode() {
        return Objects.hash(amount, durationMillis);
    }

    @Override
    public String toString() {
        return "AmountPerDuration@" + Integer.toHexString(hashCode()) +
                "{limit=" + amount + ", duration=" + durationMillis + '}';
    }

    private static class SerializationProxy implements Serializable{

        private static final long serialVersionUID = 9081726354000000011L;

        private final long amount;
        private final long durationMillis;
        private final long timeCreatedMillis;

        public SerializationProxy(AmountPerDuration candidate){
            this.amount = candidate.amount;
            this.durationMillis = candidate.durationMillis;
            this.timeCreatedMillis = candidate.timeCreatedMillis;
        }
        private Object readResolve() throws InvalidObjectException {
            return new AmountPerDuration(amount, durationMillis, timeCreatedMillis);
        }
    }

    private Object writeReplace(){ return new SerializationProxy(this); }
    private void readObject(ObjectInputStream ois) throws InvalidObjectException {
        throw new InvalidObjectException("Secure proxy must be used for serialization");
    }
}
