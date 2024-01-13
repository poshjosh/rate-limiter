package io.github.poshjosh.ratelimiter;

import io.github.poshjosh.ratelimiter.annotations.Beta;
import io.github.poshjosh.ratelimiter.bandwidths.Bandwidth;
import io.github.poshjosh.ratelimiter.bandwidths.BandwidthState;
import io.github.poshjosh.ratelimiter.bandwidths.Bandwidths;

import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

final class RateLimiters implements RateLimiter {

    static RateLimiter of(RateLimiter... rateLimiters) {
        return new RateLimiters(rateLimiters);
    }

    private final RateLimiter[] rateLimiters;

    private RateLimiters(RateLimiter[] rateLimiters) {
        this.rateLimiters = Objects.requireNonNull(rateLimiters);
    }

    @Override
    @Beta
    public BandwidthState getBandwidth() {
        // We do a cast from BandwidthState to Bandwidth - a bad assumption
        // TODO - Find a better way to do this
        return Arrays.stream(rateLimiters)
                .map(RateLimiter::getBandwidth)
                .reduce((one, two) -> Bandwidths.of((Bandwidth)one, (Bandwidth)two))
                .orElseThrow(() -> new IllegalStateException("No bandwidths found"));
    }

    @Override
    public double acquire(int permits) {
        double totalTime = 0;
        for (RateLimiter rateLimiter : rateLimiters) {
            final double timeSpent = rateLimiter.acquire(permits);
            if (timeSpent > 0) {
                totalTime += timeSpent;
            }
        }
        return totalTime;
    }

    @Override
    public boolean tryAcquire(int permits, long timeout, TimeUnit unit) {
        int successCount = 0;
        for (RateLimiter rateLimiter : rateLimiters) {
            // We need to call all tryAcquire methods to ensure that the permits are reserved
            if (rateLimiter.tryAcquire(permits, timeout, unit)) {
                ++successCount;
            }
        }
        return successCount == rateLimiters.length;
    }

    @Override
    public String toString() {
        return "RateLimiters{" + rateLimiters.length + "=" + Arrays.toString(rateLimiters) + '}';
    }
}
