package io.github.poshjosh.ratelimiter.model;

import java.lang.annotation.Annotation;
import java.util.Optional;

/**
 * A source of rate limiting information. E.g a class, a method etc
 */
public interface RateSource {

    static RateSource of(String id, boolean isRateLimited) {
        return new SimpleRateSource(id, isRateLimited);
    }

    Object getSource();
    String getId();
    <T extends Annotation> Optional<T> getAnnotation(Class<T> annotationClass);
    boolean isRateLimited();
    default boolean isGroupType() { return false; }
    default boolean isOwnDeclarer() {
        return getDeclarer().orElse(null) == this;
    }
    default Optional<RateSource> getDeclarer() { return Optional.empty(); }
}
