package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.LimitWithinDuration;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.util.RateFactory;

import java.lang.reflect.Method;
import java.util.*;

public class RateFactoryForMethodLevelAnnotation<K> implements RateFactory<K> {

    private final List<Class<?>> restResourceClasses;
    private final AnnotatedElementIdProvider<Method, K> methodAnnotatedElementIdProvider;

    public RateFactoryForMethodLevelAnnotation(List<Class<?>> restResourceClasses,
                                               AnnotatedElementIdProvider<Method, K> methodAnnotatedElementIdProvider) {
        this.restResourceClasses = Objects.requireNonNull(restResourceClasses);
        this.methodAnnotatedElementIdProvider = Objects.requireNonNull(methodAnnotatedElementIdProvider);
    }

    @Override
    public Map<K, Rate> getRates() {
        final Map<K, Rate> rates = new HashMap<>();
        for (Class<?> controllerClass : restResourceClasses) {
            addRates(controllerClass, rates);
        }
        return rates.isEmpty() ? Collections.emptyMap() : Collections.unmodifiableMap(rates);
    }

    private void addRates(Class<?> controllerClass, Map<K, Rate> addTo){

        final Method[] methods = controllerClass.getMethods();

        for (Method method : methods) {

            RateLimit rateLimit = method.getAnnotation(RateLimit.class);

            if (rateLimit != null) {

                final Rate methodRate = new LimitWithinDuration(rateLimit.limit(), rateLimit.duration());

                final K key = methodAnnotatedElementIdProvider.getId(method);

                addTo.put(key, methodRate);
            }
        }
    }
}
