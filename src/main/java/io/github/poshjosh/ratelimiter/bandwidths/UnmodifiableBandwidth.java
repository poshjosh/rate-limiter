package io.github.poshjosh.ratelimiter.bandwidths;

import java.util.Objects;

final class UnmodifiableBandwidth implements Bandwidth {
    private final Bandwidth delegate;
    UnmodifiableBandwidth(Bandwidth delegate) {
        this.delegate = Objects.requireNonNull(delegate);
    }

    @Override
    public Bandwidth with(long nowMicros) {
        return delegate.with(nowMicros);
    }

    @Override
    public boolean isAvailable(long nowMicros, long timeoutMicros) {
        return delegate.isAvailable(nowMicros, timeoutMicros);
    }

    @Override
    public long reserveAndGetWaitLength(int permits, long nowMicros) {
        throw new UnsupportedOperationException("This Bandwidth is unmodifiable");
    }

    @Override
    public long reserveEarliestAvailable(int permits, long nowMicros) {
        throw new UnsupportedOperationException("This Bandwidth is unmodifiable");
    }

    @Override
    public double getPermitsPerSecond() {
        return delegate.getPermitsPerSecond();
    }

    @Override
    public long queryEarliestAvailable(long nowMicros) {
        return delegate.queryEarliestAvailable(nowMicros);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        UnmodifiableBandwidth that = (UnmodifiableBandwidth) o;
        return Objects.equals(delegate, that.delegate);
    }

    @Override
    public int hashCode() {
        return Objects.hash(delegate);
    }

    @Override
    public String toString() {
        return "UnmodifiableBandwidth{" + "delegate=" + delegate + '}';
    }
}
