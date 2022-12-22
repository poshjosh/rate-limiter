package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.bandwidths.SmoothBandwidth;

import java.lang.ref.WeakReference;
import java.lang.reflect.InvocationTargetException;
import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

public abstract class BandwidthFactory {

    public static final class SmoothBurstyBandwidthFactory extends BandwidthFactory {
        private final double maxBurstsSeconds;
        public SmoothBurstyBandwidthFactory() {
            this(1.0);
        }
        public SmoothBurstyBandwidthFactory(double maxBurstsSeconds) {
            this.maxBurstsSeconds = maxBurstsSeconds;
        }
        @Override
        public Bandwidth createNew(double permitsPerSecond, long nowMicros) {
            return SmoothBandwidth.bursty(permitsPerSecond, nowMicros, maxBurstsSeconds);
        }
    }

    public static final class SmoothWarmingUpBandwidthFactory extends BandwidthFactory {
        private final long warmupPeriod;
        private final TimeUnit timeUnit;
        private final double coldFactor;
        public SmoothWarmingUpBandwidthFactory() {
            this(1, TimeUnit.SECONDS, 3.0);
        }
        public SmoothWarmingUpBandwidthFactory(long warmupPeriod, TimeUnit timeUnit, double coldFactor) {
            this.warmupPeriod = warmupPeriod;
            this.timeUnit = Objects.requireNonNull(timeUnit);
            this.coldFactor = coldFactor;
        }
        @Override
        public Bandwidth createNew(double permitsPerSecond, long nowMicros) {
            return SmoothBandwidth.warmingUp(permitsPerSecond, nowMicros, warmupPeriod, timeUnit, coldFactor);
        }
    }

    public static BandwidthFactory bursty() {
        return new SmoothBurstyBandwidthFactory();
    }

    public static BandwidthFactory bursty(double maxBurstsSeconds) {
        return new SmoothBurstyBandwidthFactory(maxBurstsSeconds);
    }

    public static BandwidthFactory warmingUp() {
        return new SmoothWarmingUpBandwidthFactory();
    }

    public static BandwidthFactory warmingUp(long warmupPeriod, TimeUnit timeUnit, double coldFactor) {
        return new SmoothWarmingUpBandwidthFactory(warmupPeriod, timeUnit, coldFactor);
    }

    protected BandwidthFactory() { }

    public abstract Bandwidth createNew(double permitsPerSecond, long nowMicros);

    public Bandwidth createNew(double permitsPerSecond) {
        return createNew(permitsPerSecond, 0);
    }

    public Bandwidth createNew(long amount, Duration duration) {
        return createNew(amount, duration.toNanos(), TimeUnit.NANOSECONDS);
    }
    public Bandwidth createNew(long amount, Duration duration, long nowMicros) {
        return createNew(amount, duration.toNanos(), TimeUnit.NANOSECONDS, nowMicros);
    }

    public Bandwidth createNew(long amount, long duration, TimeUnit timeUnit) {
        return createNew(amount, duration, timeUnit, 0);
    }
    public Bandwidth createNew(long amount, long duration, TimeUnit timeUnit, long nowMicros) {
        return createNew(toPermitsPerSecond(amount, duration, timeUnit), nowMicros);
    }

    protected double toPermitsPerSecond(final long amount, final long duration, final TimeUnit timeUnit) {
        // We use the highest precision
        final long nanosDuration = timeUnit.toNanos(duration);
        final double perNanos = (double)amount / nanosDuration;
        // Won't work because it will return zero if the result is a fraction
        //SECONDS.convert((long)perNanos, NANOSECONDS);
        return perNanos * TimeUnit.SECONDS.toNanos(1L);
    }

    private static WeakReference<Map<String, BandwidthFactory>> bandwidthFactoryMapWeakReference;
    public static <T extends BandwidthFactory> BandwidthFactory getOrCreateBandwidthFactory(Class<T> clazz) {
        if (bandwidthFactoryMapWeakReference == null || bandwidthFactoryMapWeakReference.get() == null) {
            bandwidthFactoryMapWeakReference = new WeakReference<>(new HashMap<>());
        }
        Map<String, BandwidthFactory> bandwidthFactoryMap = bandwidthFactoryMapWeakReference.get();
        if (bandwidthFactoryMap == null) {
            bandwidthFactoryMap = new HashMap<>();
            bandwidthFactoryMapWeakReference = new WeakReference<>(bandwidthFactoryMap);
        }
        final String key = clazz.getName();
        BandwidthFactory bandwidthFactory = bandwidthFactoryMap.get(key);
        if (bandwidthFactory == null) {
            try {
                bandwidthFactory = (BandwidthFactory)clazz.getConstructors()[0].newInstance();
                bandwidthFactoryMap.put(key, bandwidthFactory);
            } catch (InstantiationException | IllegalAccessException |
                    IllegalArgumentException | InvocationTargetException e) {
                throw new RuntimeException(e);
            }
        }
        return bandwidthFactory;
    }
}
