package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.LimitWithinDuration;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.util.RateFactory;

import java.util.*;

public class RateFactoryForClassLevelAnnotation<K> implements RateFactory<K> {

    private final List<Class<?>> targetClasses;
    private final AnnotatedElementIdProvider<Class<?>, K> classAnnotatedElementIdProvider;

    public RateFactoryForClassLevelAnnotation(List<Class<?>> targetClasses,
                                              AnnotatedElementIdProvider<Class<?>, K> classAnnotatedElementIdProvider) {
        this.targetClasses = Objects.requireNonNull(targetClasses);
        this.classAnnotatedElementIdProvider = Objects.requireNonNull(classAnnotatedElementIdProvider);
    }

    @Override
    public Map<K, Rate[]> getRates() {

        final Map<K, Rate[]> rateMap = new HashMap<>();

        for (Class<?> clazz : targetClasses) {

            Rate [] rates = getRates(clazz);

            if(rates.length > 0) {

                final K key = classAnnotatedElementIdProvider.getId(clazz);

                rateMap.put(key, rates);
            }
        }

        return rateMap.isEmpty() ? Collections.emptyMap() : Collections.unmodifiableMap(rateMap);
    }

    private Rate[] getRates(Class<?> clazz){
        final RateLimit [] rateLimitArray = clazz.getAnnotationsByType(RateLimit.class);
        if(rateLimitArray != null && rateLimitArray.length > 0) {
            final Rate [] rates = new Rate[rateLimitArray.length];
            for(int i=0; i<rateLimitArray.length; i++) {
                rates[i] = new LimitWithinDuration(rateLimitArray[i].limit(), rateLimitArray[i].duration());
            }
            return rates;
        }else{
            return new Rate[0];
        }
    }
}
