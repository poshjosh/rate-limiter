package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.util.RateFactory;

import java.util.*;

public class RateFactoryForClassLevelAnnotation<K> implements RateFactory<K> {

    private final List<Class<?>> targetClasses;
    private final AnnotatedElementIdProvider<Class<?>, K> annotatedClassIdProvider;

    public RateFactoryForClassLevelAnnotation(List<Class<?>> targetClasses,
                                              AnnotatedElementIdProvider<Class<?>, K> annotatedClassIdProvider) {
        this.targetClasses = Objects.requireNonNull(targetClasses);
        this.annotatedClassIdProvider = Objects.requireNonNull(annotatedClassIdProvider);
    }

    @Override
    public List<RateComposition<K>> getRates() {

        final List<RateComposition<K>> rateCompositionList = new ArrayList<>();

        for (Class<?> clazz : targetClasses) {
            do {

                final RateComposition<K> rateComposition = getRateCompositionOrNull(clazz);

                if(rateComposition != null) {

                    rateCompositionList.add(rateComposition);
                }

                clazz = clazz.getSuperclass();

            }while(clazz != null && !clazz.equals(Object.class));
        }

        return rateCompositionList.isEmpty() ? Collections.emptyList() : Collections.unmodifiableList(rateCompositionList);
    }

    private RateComposition<K> getRateCompositionOrNull(Class<?> clazz){
        final K key = annotatedClassIdProvider.getId(clazz);
        if(key == null) {
            return null;
        }
        final RateLimit [] rateLimitArray = clazz.getAnnotationsByType(RateLimit.class);
        if(rateLimitArray.length < 1) {
            return null;
        }
        final Rate [] rates = Util.createRates(rateLimitArray);
        return new RateComposition<K>().id(key).rates(rates);
    }
}
