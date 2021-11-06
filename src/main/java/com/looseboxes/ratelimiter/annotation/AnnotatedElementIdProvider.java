package com.looseboxes.ratelimiter.annotation;

/**
 * Map an annotated element (e.g class, method) to an ID.
 * @param <SOURCE>
 * @param <ID>
 */
public interface AnnotatedElementIdProvider<SOURCE, ID> {
    ID getId(SOURCE source);
}
