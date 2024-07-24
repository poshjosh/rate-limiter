package io.github.poshjosh.ratelimiter.bandwidths;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

public final class BandwidthFactories {

    private static WeakReference<Map<String, BandwidthFactory>> bandwidthFactoryMapWeakReference;

    private BandwidthFactories() { }

    static BandwidthFactory createSystemBandwidthFactory() {
        return createBandwidthFactory(getSystemBandwidthFactoryClass());
    }

    private static Class<? extends BandwidthFactory> getSystemBandwidthFactoryClass() {
        final String factoryClassName = System.getProperty("bandwidth-factory-class");
        if (factoryClassName == null) {
            return AllOrNothing.class;
        }
        try {
            return (Class<BandwidthFactory>)Class.forName(factoryClassName);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    static <T extends BandwidthFactory> T getOrCreateBandwidthFactory(Class<T> clazz) {
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
            bandwidthFactory = createBandwidthFactory(clazz);
            bandwidthFactoryMap.put(key, bandwidthFactory);
        }
        return (T)bandwidthFactory;
    }

    private static <T extends BandwidthFactory> T createBandwidthFactory(Class<T> clazz) {
        try {
            return clazz.newInstance();
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException e) {
            throw new RuntimeException(e);
        }
    }

    public static BandwidthFactory getDefault() {
        return getOrCreateBandwidthFactory(getDefaultClass());
    }

    public static Class<? extends BandwidthFactory> getDefaultClass() {
        return BandwidthFactories.Default.class;
    }

    public static BandwidthFactory bursty() {
        return getOrCreateBandwidthFactory(SmoothBursty.class);
    }

    public static BandwidthFactory bursty(double maxBurstsSeconds) {
        return new SmoothBursty(maxBurstsSeconds);
    }

    public static BandwidthFactory warmingUp() { return getOrCreateBandwidthFactory(SmoothWarmingUp.class); }

    public static BandwidthFactory warmingUp(long warmupPeriod, TimeUnit timeUnit,
            double coldFactor) {
        return new SmoothWarmingUp(warmupPeriod, timeUnit, coldFactor);
    }

    public static BandwidthFactory allOrNothing() {
        return getOrCreateBandwidthFactory(AllOrNothing.class);
    }

    public static final class Default implements BandwidthFactory {
        // This is initialized from a system property. We want to use the value at start up and not change
        // so we do static initialization.
        private static final BandwidthFactory delegate = createSystemBandwidthFactory();
        public Default() { }
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
