package com.looseboxes.ratelimiter.builder;

import java.util.List;

public interface RateLimiterCollectionBuilder<R> {

    R build(Class<?> clazz);

    R build(Class<?>... classes);

    R build(List<Class<?>> classes);
}
