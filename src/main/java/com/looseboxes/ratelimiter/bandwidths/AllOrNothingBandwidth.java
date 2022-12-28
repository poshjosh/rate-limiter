package com.looseboxes.ratelimiter.bandwidths;

import com.looseboxes.ratelimiter.Checks;
import com.looseboxes.ratelimiter.annotations.Beta;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * An all or nothing Bandwidth, wired to return either zero or maximum possible value from both
 * {@link #queryEarliestAvailable(long)} and {@link #reserveEarliestAvailable(int, long)}
 *
 * Beta
 */
@Beta
final class AllOrNothingBandwidth implements Bandwidth, Serializable {

    private static final long serialVersionUID = 90L;

    private static final class Rate implements Comparable<Rate>, Serializable {

        private static final long serialVersionUID = 10L;

        private long limit;
        private long durationMicros;
        private long nowMicros;

        private Rate(long permits, long durationMicros, long nowMicros) {
            Checks.requireNotNegative(permits, "permits");
            Checks.requireNotNegative(durationMicros, "duration");
            this.limit = permits;
            this.durationMicros = durationMicros;
            this.nowMicros = nowMicros;
        }
        public int compareTo(Rate rate) {
            if(limit == rate.limit && durationMicros == rate.durationMicros) {
                return 0;
            }
            if (limit >= rate.limit) {
                return durationMicros > rate.durationMicros ? 0 : 1;
            }
            return -1;
        }
        private void reset(long nowMicros) {
            this.limit = 0;
            this.durationMicros = 0;
            this.nowMicros = nowMicros;
        }
        private void increment(int amount, long nowMicros) {
            Checks.requireNotNegative(amount, "amount");
            this.limit += amount;
            this.durationMicros = (nowMicros - this.nowMicros);
        }
        @Override
        public String toString() {
            return "Rate{limit=" + limit + ", duration=" + durationMicros/1000 + "milli, now=" + nowMicros/1000 + "milli}";
        }
    }

    private final Rate limit;
    private final Rate rate;

    AllOrNothingBandwidth(long permits, long duration, TimeUnit timeUnit, long nowMicros) {
        this(
                new Rate(permits, TimeUnit.MICROSECONDS.convert(duration, timeUnit), nowMicros),
                new Rate(0, 0, nowMicros)
        );
    }

    private AllOrNothingBandwidth(Rate limit, Rate rate) {
        this.limit = Objects.requireNonNull(limit);
        this.rate = Objects.requireNonNull(rate);
    }

    @Override
    public void setRate(double permitsPerSecond, long nowMicros) {
        throw new UnsupportedOperationException();
    }

    @Override
    public double getRate() {
        return (double)(limit.limit * TimeUnit.MICROSECONDS.toSeconds(1)) / limit.durationMicros;
    }

    @Override
    public long queryEarliestAvailable(long nowMicros) {

        rate.increment(0, nowMicros);

        final int comparison = rate.compareTo(limit);
        //System.out.printf("%s AllOrNothingBandwidth comparison: %d, lhs: %s, rhs: %s\n",
        //        java.time.LocalTime.now(), comparison, rate, limit);
        if (comparison == 0) {
            rate.reset(nowMicros);
        }

        if (comparison <= 0) {
            return 0;
        }

        return Long.MAX_VALUE;
    }

    @Override
    public long reserveEarliestAvailable(int permits, long nowMicros) {
        long earliestAvailable = queryEarliestAvailable(nowMicros);
        rate.increment(permits, nowMicros);
        return earliestAvailable;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SecureSerializationProxy that = (SecureSerializationProxy) o;
        return limit.equals(that.limit) && rate.equals(that.rate);
    }

    @Override
    public int hashCode() {
        return Objects.hash(limit, rate);
    }

    @Override
    public String toString() {
        return "AllOrNothingBandwidth{" + "limit=" + limit + ", rate=" + rate + '}';
    }

    private static class SecureSerializationProxy implements Serializable {

        private static final long serialVersionUID = 91L;

        private final Rate limit;
        private final Rate rate;

        public SecureSerializationProxy(AllOrNothingBandwidth candidate){
            this.limit = candidate.limit;
            this.rate = candidate.rate;
        }
        private Object readResolve() throws InvalidObjectException {
            return new AllOrNothingBandwidth(limit, rate);
        }
    }

    private Object writeReplace(){
        return new AllOrNothingBandwidth.SecureSerializationProxy(this);
    }
    private void readObject(ObjectInputStream ois) throws InvalidObjectException {
        throw new InvalidObjectException("Secure proxy must be used for serialization");
    }
}