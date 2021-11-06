package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.LimitWithinDuration;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.util.RateFactory;

import java.lang.reflect.Method;
import java.util.*;

public class RateFactoryForMethodLevelAnnotation<K> implements RateFactory<K> {

    private final List<Class<?>> targetClasses;
    private final AnnotatedElementIdProvider<Method, K> methodAnnotatedElementIdProvider;

    public RateFactoryForMethodLevelAnnotation(List<Class<?>> targetClasses,
                                               AnnotatedElementIdProvider<Method, K> methodAnnotatedElementIdProvider) {
        this.targetClasses = Objects.requireNonNull(targetClasses);
        this.methodAnnotatedElementIdProvider = Objects.requireNonNull(methodAnnotatedElementIdProvider);
    }

    @Override
    public Map<K, Rate[]> getRates() {
        final Map<K, Rate[]> rates = new HashMap<>();
        for (Class<?> clazz : targetClasses) {
            addRates(clazz, rates);
        }
        return rates.isEmpty() ? Collections.emptyMap() : Collections.unmodifiableMap(rates);
    }

    private void addRates(Class<?> clazz, Map<K, Rate[]> addTo){

        final Method[] methods = clazz.getMethods();

        for (Method method : methods) {

            RateLimit [] rateLimitArray = method.getAnnotationsByType(RateLimit.class);

            if (rateLimitArray != null && rateLimitArray.length > 0) {

                Rate [] rates = new Rate[rateLimitArray.length];

                for(int i=0; i< rateLimitArray.length; i++) {

                    rates[i] = new LimitWithinDuration(rateLimitArray[i].limit(), rateLimitArray[i].duration());
                }

                final K key = methodAnnotatedElementIdProvider.getId(method);

                addTo.put(key, rates);
            }
        }
    }
}
