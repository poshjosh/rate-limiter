package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.util.RateFactory;

import java.lang.reflect.Method;
import java.util.*;

public class MethodLevelAnnotationRateFactory<K> implements RateFactory<K> {

    private final List<Class<?>> targetClasses;
    private final AnnotatedElementIdProvider<Method, K> annotatedMethodIdProvider;

    public MethodLevelAnnotationRateFactory(List<Class<?>> targetClasses,
                                            AnnotatedElementIdProvider<Method, K> annotatedMethodIdProvider) {
        this.targetClasses = Objects.requireNonNull(targetClasses);
        this.annotatedMethodIdProvider = Objects.requireNonNull(annotatedMethodIdProvider);
    }

    @Override
    public List<RateComposition<K>> getRates() {

        final List<RateComposition<K>> rates = new ArrayList<>();

        for (Class<?> clazz : targetClasses) {

            do {

                // We only get the methods of the current class (i.e not inherited methods) since we
                // are recursively calling getSuperClass()
                final Method[] methods = clazz.getDeclaredMethods();

                addRates(methods, rates);

                clazz = clazz.getSuperclass();

            }while(clazz != null && !clazz.equals(Object.class));
        }

        return rates.isEmpty() ? Collections.emptyList() : Collections.unmodifiableList(rates);
    }

    private void addRates(Method[] methods, List<RateComposition<K>> addTo){

        for (Method method : methods) {

            final K key = annotatedMethodIdProvider.getId(method);

            if(key == null) {
                continue;
            }

            final RateLimit [] rateLimitArray = method.getAnnotationsByType(RateLimit.class);
            if(rateLimitArray.length < 1) {
                continue;
            }

            final Rate [] rates = Util.createRates(rateLimitArray);

            final RateComposition<K> rateComposition = new RateComposition<K>().id(key).rates(rates);

            addTo.add(rateComposition);
        }
    }
}
