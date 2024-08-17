package io.github.poshjosh.ratelimiter.bandwidths;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

public final class BandwidthFactories {

    private static final Map<String, BandwidthFactory> bandwidthFactoryMap = new HashMap<>();

    private BandwidthFactories() { }

    static BandwidthFactory createSystemBandwidthFactory() {
        final String className = System.getProperty("bandwidth-factory-class");
        return createBandwidthFactory(loadClassOrDefault(className));
    }

    public static <T extends BandwidthFactory> T getOrCreateBandwidthFactory
            (/* Nullable*/ String className) {
        BandwidthFactory bandwidthFactory = bandwidthFactoryMap.get(className);
        if (bandwidthFactory == null) {
            bandwidthFactory = createBandwidthFactory(loadClassOrDefault(className));
            bandwidthFactoryMap.put(className, bandwidthFactory);
        }
        return (T)bandwidthFactory;
    }

    public static <T extends BandwidthFactory> T getOrCreateBandwidthFactory(Class<T> clazz) {
        final String key = clazz.getName();
        BandwidthFactory bandwidthFactory = bandwidthFactoryMap.get(key);
        if (bandwidthFactory == null) {
            bandwidthFactory = createBandwidthFactory(clazz);
            bandwidthFactoryMap.put(key, bandwidthFactory);
        }
        return (T)bandwidthFactory;
    }

    public static BandwidthFactory ofDefaults() {
        return getOrCreateBandwidthFactory(getDefaultClass());
    }

    public static Class<? extends BandwidthFactory> getDefaultClass() {
        return BandwidthFactories.Default.class;
    }

    public static BandwidthFactory ofBursty() {
        return getOrCreateBandwidthFactory(SmoothBursty.class);
    }

    public static BandwidthFactory ofBursty(double maxBurstsSeconds) {
        return new SmoothBursty(maxBurstsSeconds);
    }

    public static BandwidthFactory ofWarmingUp() { return getOrCreateBandwidthFactory(SmoothWarmingUp.class); }

    public static BandwidthFactory ofWarmingUp(long warmupPeriod, TimeUnit timeUnit,
            double coldFactor) {
        return new SmoothWarmingUp(warmupPeriod, timeUnit, coldFactor);
    }

    public static BandwidthFactory ofAllOrNothing() {
        return getOrCreateBandwidthFactory(AllOrNothing.class);
    }

    private static <T extends BandwidthFactory> Class<T> loadClassOrDefault(String className) {
        if (className == null || className.isEmpty()) {
            return (Class<T>)AllOrNothing.class;
        }
        try {
            return (Class<T>)Class.forName(className);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    private static <T extends BandwidthFactory> T createBandwidthFactory(Class<T> clazz) {
        try {
            return clazz.newInstance();
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException e) {
            throw new RuntimeException(e);
        }
    }

    public static final class Default implements BandwidthFactory {
        // This is initialized from a system property. We want to use the value at start up and not change
        // so we do static initialization.
        private static final BandwidthFactory delegate = createSystemBandwidthFactory();
        @Override
        public Bandwidth createNew(long permits, long duration, TimeUnit timeUnit, long nowMicros) {
            return delegate.createNew(permits, duration, timeUnit, nowMicros);
        }
        @Override
        public String toString() {
            return "BandwidthFactory$Default{delegate=" + delegate + "}";
        }
    }

    public static final class SmoothBursty implements BandwidthFactory {
        private final double maxBurstsSeconds;
        public SmoothBursty() {
            this(1.0);
        }
        public SmoothBursty(double maxBurstsSeconds) {
            this.maxBurstsSeconds = maxBurstsSeconds;
        }
        @Override
        public Bandwidth createNew(long permits, long duration, TimeUnit timeUnit, long nowMicros) {
            return createNew(SmoothWarmingUp.toPermitsPerSecond(permits, duration, timeUnit), nowMicros);
        }
        private Bandwidth createNew(double permitsPerSecond, long nowMicros) {
            return Bandwidths.bursty(permitsPerSecond, nowMicros, maxBurstsSeconds);
        }
        @Override
        public String toString() { return "BandwidthFactory$SmoothBursty{maxBurstsSeconds=" + maxBurstsSeconds + '}'; }
    }

    public static final class SmoothWarmingUp implements BandwidthFactory {
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
        public Bandwidth createNew(long permits, long duration, TimeUnit timeUnit, long nowMicros) {
            return createNew(toPermitsPerSecond(permits, duration, timeUnit), nowMicros);
        }
        private Bandwidth createNew(double permitsPerSecond, long nowMicros) {
            return Bandwidths.warmingUp(permitsPerSecond, nowMicros, warmupPeriod, timeUnit, coldFactor);
        }
        private static double toPermitsPerSecond(final long permits, final long duration, final TimeUnit timeUnit) {
            // We use the highest precision
            final long nanosDuration = timeUnit.toNanos(duration);
            final double perNanos = (double)permits / nanosDuration;
            // Won't work because it will return zero if the result is a fraction
            //SECONDS.convert((long)perNanos, NANOSECONDS);
            return perNanos * TimeUnit.SECONDS.toNanos(1L);
        }
        @Override
        public String toString() {
            return "BandwidthFactory$SmoothWarmingUp{warmupPeriod=" + warmupPeriod +
                    ", timeUnit=" + timeUnit + ", coldFactor=" + coldFactor + '}';
        }
    }

    public static final class AllOrNothing implements BandwidthFactory {
        @Override
        public Bandwidth createNew(long permits, long duration, TimeUnit timeUnit, long nowMicros) {
            return Bandwidths.allOrNothing(permits, duration, timeUnit, nowMicros);
        }
        @Override
        public String toString() {
            return "BandwidthFactory$AllOrNothing{}";
        }
    }
}
