package io.github.poshjosh.ratelimiter.bandwidths;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * An all or nothing Bandwidth, wired to return a value between zero and stable interval for both methods
 * {@link #queryEarliestAvailable(long)} and {@link #reserveEarliestAvailable(int, long)}
 */
final class AllOrNothingBandwidth implements Bandwidth, Serializable {

    private static final long serialVersionUID = 90L;

    private static final class Rate implements Comparable<Rate>, Serializable {

        private static final long serialVersionUID = 10L;

        private long permits;
        private long durationMicros;
        private long nowMicros;

        private Rate(long permits, long durationMicros, long nowMicros) {
            Checks.requireNotNegative(permits, "permits");
            Checks.requireNotNegative(durationMicros, "duration");
            this.permits = permits;
            this.durationMicros = durationMicros;
            this.nowMicros = nowMicros;
        }
        public int compareTo(Rate rate) {
            if(permits == rate.permits && durationMicros == rate.durationMicros) {
                return 0;
            }
            if (permits >= rate.permits) {
                return durationMicros > rate.durationMicros ? 0 : 1;
            }
            return -1;
        }
        private void reset(long nowMicros) {
            this.permits = 0;
            this.durationMicros = 0;
            this.nowMicros = nowMicros;
        }
        private void increment(int amount, long nowMicros) {
            Checks.requireNotNegative(amount, "amount");
            this.permits += amount;
            this.durationMicros = (nowMicros - this.nowMicros);
        }
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Rate rate = (Rate) o;
            return permits == rate.permits && durationMicros == rate.durationMicros && nowMicros == rate.nowMicros;
        }
        @Override
        public int hashCode() {
            return Objects.hash(permits, durationMicros, nowMicros);
        }
        @Override
        public String toString() {
            return "{permits=" + permits + ", duration=" + (durationMicros/1000) +
                    "millis, now=" + (nowMicros/1000) + "millis}";
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
    public AllOrNothingBandwidth with(long nowMicros) {
        return new AllOrNothingBandwidth(
                new Rate(limit.permits, limit.durationMicros, nowMicros),
                new Rate(rate.permits, rate.durationMicros, nowMicros)
        );
    }

    @Override
    public double getPermitsPerSecond() {
        double permitsPerMicro = (double)(limit.permits) / limit.durationMicros;
        return permitsPerMicro * TimeUnit.SECONDS.toMicros(1);
    }

    @Override
    public long queryEarliestAvailable(long nowMicros) {
        final int comparison = compare(nowMicros);
        return toWaitTime(nowMicros, comparison);
    }

    @Override
    public long reserveEarliestAvailable(int permits, long nowMicros) {
        final int comparison = compare(nowMicros);
        if (comparison == 0) {
            rate.reset(nowMicros);
        }
        long earliestAvailable = toWaitTime(nowMicros, comparison);
        rate.increment(permits, nowMicros);
        return earliestAvailable;
    }

    private int compare(long nowMicros) {
        rate.increment(0, nowMicros);
        return rate.compareTo(limit);
    }

    private long toWaitTime(long nowMicros, int comparison) {
        return comparison <= 0 ? nowMicros : nowMicros + (limit.durationMicros - rate.durationMicros);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AllOrNothingBandwidth that = (AllOrNothingBandwidth) o;
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
