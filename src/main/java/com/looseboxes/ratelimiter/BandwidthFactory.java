package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.bandwidths.SmoothBandwidth;

import java.lang.ref.WeakReference;
import java.lang.reflect.InvocationTargetException;
import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public abstract class BandwidthFactory {

    public static final class SmoothBurstyBandwidthFactory extends BandwidthFactory {
        @Override
        public Bandwidth createNew(double permitsPerSecond) {
            return SmoothBandwidth.bursty(permitsPerSecond);
        }
    }

    public static final class SmoothWarmingUpBandwidthFactory extends BandwidthFactory {
        @Override
        public Bandwidth createNew(double permitsPerSecond) {
            return SmoothBandwidth.warmingUp(permitsPerSecond, 5);
        }
    }

    public static BandwidthFactory bursty() {
        return new SmoothBurstyBandwidthFactory();
    }

    public abstract Bandwidth createNew(double permitsPerSecond);

    public static BandwidthFactory warmingUp() {
        return new SmoothWarmingUpBandwidthFactory();
    }

    public Bandwidth createNew(long amount, Duration duration) {
        return createNew(amount, duration.toNanos(), TimeUnit.NANOSECONDS);
    }

    public Bandwidth createNew(long amount, long duration, TimeUnit timeUnit) {
        return createNew(toPermitsPerSecond(amount, duration, timeUnit));
    }

    protected double toPermitsPerSecond(final long amount, final long duration, final TimeUnit timeUnit) {
        // We use the highest precision
        final long nanosDuration = timeUnit.toNanos(duration);
        final double perNanos = (double)amount / nanosDuration;
        // Won't work because it will return zero if the result is a fraction
        //SECONDS.convert((long)perNanos, NANOSECONDS);
        return perNanos * 1_000_000_000;
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
