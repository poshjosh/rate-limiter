package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.LimitWithinDuration;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.util.RateFactory;

import java.util.*;

public class RateFactoryForClassLevelAnnotation<K> implements RateFactory<K> {

    private final List<Class<?>> restResourceClasses;
    private final AnnotatedElementIdProvider<Class<?>, K> classAnnotatedElementIdProvider;

    public RateFactoryForClassLevelAnnotation(List<Class<?>> restResourceClasses,
                                              AnnotatedElementIdProvider<Class<?>, K> classAnnotatedElementIdProvider) {
        this.restResourceClasses = Objects.requireNonNull(restResourceClasses);
        this.classAnnotatedElementIdProvider = Objects.requireNonNull(classAnnotatedElementIdProvider);
    }

    @Override
    public Map<K, Rate> getRates() {

        final Map<K, Rate> rates = new HashMap<>();

        for (Class<?> controllerClass : restResourceClasses) {

            getRateOptional(controllerClass).ifPresent(rate -> {
                final K key = classAnnotatedElementIdProvider.getId(controllerClass);
                rates.put(key, rate);
            });
        }

        return rates.isEmpty() ? Collections.emptyMap() : Collections.unmodifiableMap(rates);
    }

    private Optional<Rate> getRateOptional(Class<?> controllerClass){
        final RateLimit rateLimit = controllerClass.getAnnotation(RateLimit.class);
        if(rateLimit != null) {
            return Optional.of(new LimitWithinDuration(rateLimit.limit(), rateLimit.duration()));
        }else{
            return Optional.empty();
        }
    }
}
