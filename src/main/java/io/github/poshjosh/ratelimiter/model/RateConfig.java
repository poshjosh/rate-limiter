package io.github.poshjosh.ratelimiter.model;

import java.util.Objects;
import java.util.Optional;

public final class RateConfig {

    private static final RateSource NO_SOURCE = RateSource.of("", false);

    public static RateConfig of(long permitsPerSecond) {
        return of(permitsPerSecond, "");
    }

    public static RateConfig of(long permitsPerSecond, String rateCondition) {
        return of(Rates.of(Rate.of(permitsPerSecond, rateCondition)));
    }

    public static RateConfig of(Rates value) {
        return new RateConfig(NO_SOURCE, value, null);
    }

    public static RateConfig of(RateSource source, Rates value, RateConfig parent) {
        return new RateConfig(source, value, parent);
    }

    private final RateSource source;
    private final Rates rates;
    private final RateConfig parent;

    private RateConfig(RateSource source, Rates rates, RateConfig parent) {
        this.source = Objects.requireNonNull(source);
        this.rates = Objects.requireNonNull(rates);
        this.parent = parent;
    }

    public RateConfig withSource(RateSource source) {
        return RateConfig.of(source, rates, parent);
    }

    public String getId() {
        return source.getId();
    }

    public RateSource getSource() {
        return source;
    }

    public Rates getRates() {
        return rates;
    }

    public Rates getRatesWithParentRatesAsFallback() {
        if (shouldDelegateToParent()) {
            return getParentOptional()
                    .map(RateConfig::getRates)
                    .orElse(rates);
        }
        return Rates.of(rates);
    }

    public Optional<RateConfig> getParentOptional() {
        return Optional.ofNullable(parent);
    }

    public boolean shouldDelegateToParent() {
        return !rates.hasLimits() && hasRateGroupAsParentSource();
    }

    private boolean hasRateGroupAsParentSource() {
        return parent != null && parent.getSource().isGroupType();
    }

    @Override public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof RateConfig)) {
            return false;
        }
        RateConfig that = (RateConfig) o;
        return source.equals(that.source) && rates.equals(that.rates);
    }

    @Override public int hashCode() {
        return Objects.hash(source, rates);
    }

    @Override public String toString() {
        return "RateConfig{" + "source=" + source + ", rates=" + rates +
                ", parent=" + (parent == null ? null : parent.getId()) + '}';
    }
}
