package com.looseboxes.ratelimiter.annotation;

/**
 * Map an annotated element (class, method) to an ID
 * @param <SOURCE> The type of the annotated elemnent e.g class/method
 * @param <ID> The type of the id
 */
public interface AnnotatedElementIdProvider<SOURCE, ID> {
    ID getId(SOURCE source);
}
