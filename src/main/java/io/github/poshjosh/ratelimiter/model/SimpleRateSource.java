package io.github.poshjosh.ratelimiter.model;

import java.lang.annotation.Annotation;
import java.util.Objects;
import java.util.Optional;

final class SimpleRateSource implements RateSource {
    private final String id;
    private final boolean isRateLimited;

    SimpleRateSource(String id, boolean isRateLimited) {
        this.id = Objects.requireNonNull(id);
        this.isRateLimited = isRateLimited;
    }

    @Override public Object getSource() {
        return this;
    }

    @Override public String getId() {
        return id;
    }

    @Override public <T extends Annotation> Optional<T> getAnnotation(Class<T> annotationClass) {
        return Optional.empty();
    }

    @Override public boolean isRateLimited() {
        return isRateLimited;
    }

    @Override public int hashCode() {
        return Objects.hashCode(getId());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof RateSource)) {
            return false;
        }
        return getId().equals(((RateSource) o).getId());
    }

    @Override
    public String toString() {
        return "SimpleRateSource{" + getId() + '}';
    }
}
