package io.github.poshjosh.ratelimiter;

import io.github.poshjosh.ratelimiter.annotations.Beta;
import io.github.poshjosh.ratelimiter.bandwidths.Bandwidth;
import io.github.poshjosh.ratelimiter.bandwidths.Bandwidths;
import io.github.poshjosh.ratelimiter.util.Operator;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

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
    public Bandwidth getBandwidth() {
        if (rateLimiters.length == 0) {
            // We are unlimited if there is no bandwidth
            return Bandwidth.UNLIMITED;
        }
        if (rateLimiters.length == 1) {
            return rateLimiters[0].getBandwidth();
        }
        List<Bandwidth> bandwidthList = Arrays.stream(rateLimiters)
                .map(RateLimiter::getBandwidth).collect(Collectors.toList());
        return Bandwidths.of(Operator.OR, bandwidthList.toArray(new Bandwidth[0]));
    }

    @Override
    public double acquire(int permits) {
        double totalTime = 0;
        for (RateLimiter rateLimiter : rateLimiters) {
            final double timeSpent = rateLimiter.acquire(permits);
            if (timeSpent > 0) { // Only increment when > 0, as some value may be negative.
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
        return "RateLimiters{" + rateLimiters.length + ", " + Arrays.toString(rateLimiters) + '}';
    }
}
