package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.model.Operator;
import io.github.poshjosh.ratelimiter.annotations.Beta;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * Compose a {@code Bandwidth} from multiple {@code Bandwidth}s.
 * For multiple Bandwidths conjugated with {@code Operator#OR}, the composed
 * Bandwidth fails if one or more of the Bandwidth's limit is exceeded.
 * For multiple Bandwidths conjugated with {@code Operator#AND}, the composed
 * Bandwidth fails only if all the Bandwidth's limits are exceeded.
 */
final class BandwidthArray implements Bandwidth, Serializable {

    static Bandwidth of(Operator operator, Bandwidth... bandwidths) {

        if (bandwidths.length == 0) {
            return Bandwidths.UNLIMITED;
        }

        if (bandwidths.length == 1 && Bandwidths.UNLIMITED.equals(bandwidths[0])) {
            return Bandwidths.UNLIMITED;
        }

        bandwidths = copyWithoutUnlimitedInstances(bandwidths);

        if (bandwidths.length == 0) {
            return Bandwidths.UNLIMITED;
        }

        if (bandwidths.length == 1) {
            return bandwidths[0];
        }

        return new BandwidthArray(operator, bandwidths);
    }

    private static Bandwidth[] copyWithoutUnlimitedInstances(Bandwidth... bandwidths) {
        final int numOfUnlimited = countUnlimitedInstances(bandwidths);
        if (numOfUnlimited == 0) {
            return Arrays.copyOf(bandwidths, bandwidths.length);
        }
        final Bandwidth[] result = new Bandwidth[bandwidths.length - numOfUnlimited];
        int skipped = 0;
        for(int i = 0; i < bandwidths.length; i++) {
            if (Bandwidths.UNLIMITED.equals(bandwidths[i])) {
                ++skipped;
                continue;
            }
            result[i - skipped] = bandwidths[i];
        }
        return result;
    }

    private static int countUnlimitedInstances(Bandwidth... bandwidths) {
        if (bandwidths.length == 0) {
            return 0;
        }
        int count = 0;
        for(Bandwidth bandwidth : bandwidths) {
            if (Bandwidths.UNLIMITED.equals(bandwidth)) {
                ++count;
            }
        }
        return count;
    }

    private static final long serialVersionUID = 20L;

    private final Operator operator;
    private final Bandwidth[] bandwidths;

    private BandwidthArray(Operator operator, Bandwidth... bandwidths) {
        this.operator = Objects.requireNonNull(operator);
        this.bandwidths = Objects.requireNonNull(bandwidths);
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

    /**
     * Reserves the requested number of permits and returns the time that those
     * permits can be used (in microseconds).
     * This implementation reserves multiple bandwidths and returns:
     * <ul>
     *     <li>for AND - the shortest wait time</li>
     *     <li>for OR - the longest wait time</li>
     * </ul>
     * The largest bandwidth will have the shortest wait time.
     * When this BandwidthArray is used in a RateLimiter, the RateLimiter will
     * succeed for AND, but fail for OR, when only one of many limits is exceeded.
     * <p>
     * AND fails only when all limits are exceeded. Therefore, for AND, we return
     * the shortest wait time which corresponds to the largest bandwidth.
     * </p>
     * @param permits The permits to reserve
     * @param nowMicros The current time in microseconds
     * @return The time in microseconds that the returned permits can be used
     */
    @Override
    public long reserveEarliestAvailable(int permits, long nowMicros) {
        final boolean AND = Operator.AND.equals(operator);
        long waitTime = -1;
        for(Bandwidth bandwidth : bandwidths) {
            final long currentWaitTime = bandwidth.reserveEarliestAvailable(permits, nowMicros);
            if (waitTime == -1) {
                waitTime = currentWaitTime;
            }
            if (AND && currentWaitTime < waitTime) {
                waitTime = currentWaitTime;
                continue;
            }
            if (!AND && currentWaitTime > waitTime) {
                waitTime = currentWaitTime;
            }
        }
        return waitTime;
    }

    /**
     * Returns the stable rate (as {@code permits per seconds}) with which an eligible
     * {@code Bandwidth} in this {@code Bandwidths} is configured with.
     * For AND we return the largest value, for OR we return the smallest value.
     */
    @Beta
    @Override
    public double getPermitsPer(TimeUnit timeUnit) {
        final boolean AND = Operator.AND.equals(operator);
        double result = -1;
        for(Bandwidth bandwidth : bandwidths) {
            double e = bandwidth.getPermitsPer(timeUnit);
            result = result == - 1 ? e : (AND ? Math.max(e, result) : Math.min(e, result));
        }
        return result;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        BandwidthArray that = (BandwidthArray) o;
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
        return "BandwidthArray{operator=" + operator +
                ", bandwidths=" + Arrays.toString(bandwidths) + "}";
    }

    private static class SecureSerializationProxy implements Serializable {

        private static final long serialVersionUID = 21L;

        private final Operator operator;
        private final Bandwidth[] members;

        public SecureSerializationProxy(BandwidthArray candidate){
            this.operator = candidate.operator;
            this.members = candidate.bandwidths;
        }
        private Object readResolve() throws InvalidObjectException {
            return new BandwidthArray(operator, members);
        }
    }

    private Object writeReplace(){
        return new BandwidthArray.SecureSerializationProxy(this);
    }
    private void readObject(ObjectInputStream ois) throws InvalidObjectException {
        throw new InvalidObjectException("Secure proxy must be used for serialization");
    }
}
