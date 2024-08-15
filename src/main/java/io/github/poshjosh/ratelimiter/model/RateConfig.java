package io.github.poshjosh.ratelimiter.model;

import java.util.Objects;

public final class RateConfig {

    public static final RateConfig NONE = RateConfig.of(RateSources.NONE, Rates.none(), null);

    public static RateConfig of(long permitsPerSecond) {
        return of(permitsPerSecond, "");
    }

    public static RateConfig of(long permitsPerSecond, String rateCondition) {
        return of(Rates.of(Rate.of(permitsPerSecond, rateCondition)));
    }

    public static RateConfig of(Rates value) {
        return new RateConfig(RateSources.NONE, value, NONE);
    }

    public static RateConfig of(RateSource source, Rates value) {
        return of(source, value, NONE);
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
        this.parent = parent == null ? NONE : parent;
    }

    public RateConfig withSource(RateSource source) {
        return RateConfig.of(source, rates, parent);
    }

    public String getId() {
        return source.getId();
    }

    public boolean isGroupType() {
        return source.isGroupType();
    }

    public RateSource getSource() {
        return source;
    }

    public Rates getRates() {
        return rates;
    }

    public Rates getRatesWithParentRatesAsFallback() {
        if (shouldDelegateToParent()) {
            return parent == null ? rates : parent.getRates();
        }
        return rates;
    }

    public RateConfig getParent() {
        return parent;
    }

    public boolean shouldDelegateToParent() {
        return !rates.isSet() && (parent != null && parent.isGroupType());
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
