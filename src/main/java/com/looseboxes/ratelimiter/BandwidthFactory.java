package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.annotations.Experimental;
import com.looseboxes.ratelimiter.bandwidths.AllOrNothingBandwidth;
import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.bandwidths.SmoothBandwidth;

import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static com.looseboxes.ratelimiter.BandwidthFactories.getOrCreateBandwidthFactory;

/**
 * A factory type for creating {@link Bandwidth}s.
 *
 * Implementations are required to have a no-argument constructor.
 */
public interface BandwidthFactory {

    final class Default implements BandwidthFactory {
        // This is initialized from a system property. We want to use the value at start up and not change
        // so we do static initialization.
        private static final BandwidthFactory delegate = BandwidthFactories.createSystemBandwidthFactory();
        public Default() { }
        @Override
        public Bandwidth createNew(double permitsPerSecond, long nowMicros) {
            return delegate.createNew(permitsPerSecond, nowMicros);
        }
        @Override
        public String toString() {
            return "BandwidthFactory.Default{delegate=" + delegate + "}";
        }
    }

    final class SmoothBursty implements BandwidthFactory {
        private final double maxBurstsSeconds;
        public SmoothBursty() {
            this(1.0);
        }
        public SmoothBursty(double maxBurstsSeconds) {
            this.maxBurstsSeconds = maxBurstsSeconds;
        }
        @Override
        public Bandwidth createNew(double permitsPerSecond, long nowMicros) {
            return SmoothBandwidth.bursty(permitsPerSecond, nowMicros, maxBurstsSeconds);
        }
        @Override
        public String toString() { return "BandwidthFactory.SmoothBursty{maxBurstsSeconds=" + maxBurstsSeconds + '}'; }
    }

    final class SmoothWarmingUp implements BandwidthFactory {
        private final long warmupPeriod;
        private final TimeUnit timeUnit;
        private final double coldFactor;
        public SmoothWarmingUp() {
            this(1, TimeUnit.SECONDS, 3.0);
        }
        public SmoothWarmingUp(long warmupPeriod, TimeUnit timeUnit, double coldFactor) {
            this.warmupPeriod = warmupPeriod;
            this.timeUnit = Objects.requireNonNull(timeUnit);
            this.coldFactor = coldFactor;
        }
        @Override
        public Bandwidth createNew(double permitsPerSecond, long nowMicros) {
            return SmoothBandwidth.warmingUp(permitsPerSecond, nowMicros, warmupPeriod, timeUnit, coldFactor);
        }
        @Override
        public String toString() {
            return "BandwidthFactory.SmoothWarmingUp{warmupPeriod=" + warmupPeriod +
                    ", timeUnit=" + timeUnit + ", coldFactor=" + coldFactor + '}';
        }
    }

    /** @Experimental */
    @Experimental
    final class AllOrNothingBursty implements BandwidthFactory {
        private final BandwidthFactory delegate;
        public AllOrNothingBursty() { delegate = getOrCreateBandwidthFactory(BandwidthFactory.SmoothBursty.class); }
        @Override
        public Bandwidth createNew(double permitsPerSecond, long nowMicros) {
            return new AllOrNothingBandwidth(delegate.createNew(permitsPerSecond, nowMicros));
        }
        @Override
        public String toString() {
            return "BandwidthFactory.AllOrNothingBursty{delegate=" + delegate + "}";
        }
    }

    /** @Experimental */
    @Experimental
    final class AllOrNothing implements BandwidthFactory {
        private final BandwidthFactory delegate;
        public AllOrNothing() {
            this(getOrCreateBandwidthFactory(BandwidthFactory.Default.class));
        }
        public AllOrNothing(BandwidthFactory delegate) {
            this.delegate = Objects.requireNonNull(delegate);
        }
        @Override
        public Bandwidth createNew(double permitsPerSecond, long nowMicros) {
            return new AllOrNothingBandwidth(delegate.createNew(permitsPerSecond, nowMicros));
        }
        @Override
        public String toString() {
            return "BandwidthFactory.AllOrNothing{delegate=" + delegate + "}";
        }
    }

    static BandwidthFactory getDefault() {
        return getOrCreateBandwidthFactory(BandwidthFactory.Default.class);
    }

    static BandwidthFactory bursty() {
        return getOrCreateBandwidthFactory(SmoothBursty.class);
    }

    static BandwidthFactory bursty(double maxBurstsSeconds) {
        return new SmoothBursty(maxBurstsSeconds);
    }

    static BandwidthFactory warmingUp() { return getOrCreateBandwidthFactory(SmoothWarmingUp.class); }

    static BandwidthFactory warmingUp(long warmupPeriod, TimeUnit timeUnit, double coldFactor) {
        return new SmoothWarmingUp(warmupPeriod, timeUnit, coldFactor);
    }

    /** @Experimental */
    @Experimental
    static BandwidthFactory allOrNothingBursty() {
        return getOrCreateBandwidthFactory(AllOrNothingBursty.class);
    }

    /** @Experimental */
    @Experimental
    static BandwidthFactory allOrNothing(BandwidthFactory bandwidthFactory) {
        return new AllOrNothing(bandwidthFactory);
    }

    Bandwidth createNew(double permitsPerSecond, long nowMicros);

    default Bandwidth createNew(double permitsPerSecond) {
        return createNew(permitsPerSecond, 0);
    }

    default Bandwidth createNew(long amount, Duration duration) {
        return createNew(amount, duration.toNanos(), TimeUnit.NANOSECONDS);
    }
    default Bandwidth createNew(long amount, Duration duration, long nowMicros) {
        return createNew(amount, duration.toNanos(), TimeUnit.NANOSECONDS, nowMicros);
    }

    default Bandwidth createNew(long amount, long duration, TimeUnit timeUnit) {
        return createNew(amount, duration, timeUnit, 0);
    }
    default Bandwidth createNew(long amount, long duration, TimeUnit timeUnit, long nowMicros) {
        return createNew(Util.toPermitsPerSecond(amount, duration, timeUnit), nowMicros);
    }
}
