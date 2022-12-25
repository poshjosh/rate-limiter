package com.looseboxes.ratelimiter.bandwidths;

import com.looseboxes.ratelimiter.annotations.Experimental;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Objects;

import static java.lang.Math.max;
import static java.util.concurrent.TimeUnit.SECONDS;

/**
 * An all or nothing Bandwidth, wired to return zero wait time whenever {@code waitTime}
 * is less than {@code stableIntervalMicros}
 * @Experimental
 */
@Experimental
public class AllOrNothingBandwidth implements Bandwidth, Serializable {

    private static final long serialVersionUID = 90L;

    private final Bandwidth delegate;
    private long offsetTime;
    private long attempts;

    public AllOrNothingBandwidth(Bandwidth delegate) {
        this(delegate, 0, 0);
    }

    private AllOrNothingBandwidth(Bandwidth delegate, long offsetTime, long attempts) {
        this.delegate = Objects.requireNonNull(delegate);
        this.offsetTime = offsetTime;
        this.attempts = attempts;
    }

    @Override
    public boolean canAcquire(long nowMicros, long timeoutMicros) {
        nowMicros += offsetTime;
        final long earliestAvailable = queryEarliestAvailable(nowMicros);
        if ((earliestAvailable - offsetTime) <= getStableIntervalMicros()) {
            ++attempts;
            offsetTime = earliestAvailable;
            if (attempts >= Math.rint(getRate())) {
                offsetTime = 0;
            }
            //System.out.printf(
            //        "%s AllOrNothingBandwidth can acquire: %b, (earliest available)%d - (timeoutMicros)%d <= (elapsedMicros)%d, permits/sec: %1.6f\n",
            //        java.time.LocalTime.now(), true, earliestAvailable, timeoutMicros, nowMicros, delegate.getRate());
            return true;
        }
        final boolean canAcquire = earliestAvailable - timeoutMicros <= nowMicros;
        //System.out.printf(
        //        "%s AllOrNothingBandwidth can acquire: %b, (earliest available)%d - (timeoutMicros)%d <= (elapsedMicros)%d, permits/sec: %1.6f\n",
        //        java.time.LocalTime.now(), canAcquire, earliestAvailable, timeoutMicros, nowMicros, delegate.getRate());
        return canAcquire;
    }

    @Override
    public long queryEarliestAvailable(long nowMicros) {
        return delegate.queryEarliestAvailable(nowMicros);
    }

    private long getStableIntervalMicros() {
        final double permitsPerSecond = delegate.getRate();
        return (long)(SECONDS.toMicros(1L) / permitsPerSecond);
    }

    @Override
    public long reserveAndGetWaitLength(int permits, long nowMicros) {
        nowMicros += offsetTime;
        final long momentAvailable = reserveEarliestAvailable(permits, nowMicros);
        return max(momentAvailable - nowMicros, 0);
    }

    @Override
    public long reserveEarliestAvailable(int permits, long nowMicros) {
        return delegate.reserveEarliestAvailable(permits, nowMicros);
    }

    @Override
    public void setRate(double permitsPerSecond, long nowMicros) {
        delegate.setRate(permitsPerSecond, nowMicros);
    }

    @Override
    public double getRate() {
        return delegate.getRate();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AllOrNothingBandwidth that = (AllOrNothingBandwidth) o;
        return offsetTime == that.offsetTime && attempts == that.attempts && delegate.equals(that.delegate);
    }

    @Override
    public int hashCode() {
        return Objects.hash(delegate, offsetTime, attempts);
    }

    @Override
    public String toString() {
        return "AllOrNothingBandwidth{" +
                "offsetTime=" + offsetTime +
                ", attempts=" + attempts +
                ", delegate=" + delegate +
                '}';
    }

    private static class SecureSerializationProxy implements Serializable {

        private static final long serialVersionUID = 91L;

        private final Bandwidth delegate;
        private final long offsetTime;
        private final long attempts;

        public SecureSerializationProxy(AllOrNothingBandwidth candidate){
            this.delegate = candidate.delegate;
            this.offsetTime = candidate.offsetTime;
            this.attempts = candidate.attempts;
        }
        private Object readResolve() throws InvalidObjectException {
            return new AllOrNothingBandwidth(delegate, offsetTime, attempts);
        }
    }

    private Object writeReplace(){
        return new AllOrNothingBandwidth.SecureSerializationProxy(this);
    }
    private void readObject(ObjectInputStream ois) throws InvalidObjectException {
        throw new InvalidObjectException("Secure proxy must be used for serialization");
    }
}
