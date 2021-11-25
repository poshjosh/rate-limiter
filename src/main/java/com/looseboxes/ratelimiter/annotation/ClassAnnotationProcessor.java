package com.looseboxes.ratelimiter.annotation;

public final class ClassAnnotationProcessor implements AnnotationProcessor<Class<?>>{

    public ClassAnnotationProcessor() { }

    @Override
    public <R> R process(Class<?> clazz, AnnotationCollector<Class<?>, R> collector) {
        do {

            collect(clazz, collector);

            clazz = clazz.getSuperclass();

        }while(clazz != null && !clazz.equals(Object.class));

        return collector.getResult();
    }

    private <R> void collect(Class<?> clazz, AnnotationCollector<Class<?>, R> collector){
        final RateLimitGroup [] rateLimitGroups = clazz.getAnnotationsByType(RateLimitGroup.class);
        for(RateLimitGroup rateLimitGroup : rateLimitGroups) {
            collector.collect(clazz, rateLimitGroup);
        }
        final RateLimit [] rateLimits = clazz.getAnnotationsByType(RateLimit.class);
        for(RateLimit rateLimit : rateLimits) {
            collector.collect(clazz, rateLimit);
        }
    }
}
