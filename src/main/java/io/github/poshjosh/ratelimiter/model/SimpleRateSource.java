package io.github.poshjosh.ratelimiter.model;

import java.lang.annotation.Annotation;
import java.util.Objects;
import java.util.Optional;

final class SimpleRateSource implements RateSource {
    private final Rates rates;

    SimpleRateSource(Rates rates) {
        this.rates = Objects.requireNonNull(rates);
    }

    @Override public String getId() {
        return rates.getId();
    }

    @Override public Object getSource() {
        return RateSources.NONE;
    }

    @Override public Rates getRates() {
        return rates;
    }

    @Override public <T extends Annotation> Optional<T> getAnnotation(Class<T> annotationClass) {
        return Optional.empty();
    }

    @Override public boolean isRateLimited() {
        return this.rates.isSet();
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
