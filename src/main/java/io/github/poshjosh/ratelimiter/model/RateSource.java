package io.github.poshjosh.ratelimiter.model;

import java.lang.annotation.Annotation;
import java.lang.reflect.GenericDeclaration;
import java.util.Optional;

/**
 * A source of rate limiting information. E.g a class, a method etc
 */
public interface RateSource {
    String getId();
    Object getSource();
    Rates getRates();
    <T extends Annotation> Optional<T> getAnnotation(Class<T> annotationClass);
    default boolean isOwnDeclarer() {
        return getDeclarer().orElse(null) == this;
    }
    default Optional<RateSource> getDeclarer() { return Optional.empty(); }

    boolean isRateLimited();
    default boolean isGroupType() { return false; }

    default boolean isGenericDeclaration() { return getSource() instanceof GenericDeclaration; }
}
