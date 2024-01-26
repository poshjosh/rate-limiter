package io.github.poshjosh.ratelimiter.model;

import io.github.poshjosh.ratelimiter.bandwidths.BandwidthFactory;
import io.github.poshjosh.ratelimiter.util.Operator;
import io.github.poshjosh.ratelimiter.util.StringUtils;

import java.time.Duration;
import java.util.*;
import java.util.stream.Collectors;

public class Rates {

    public static Rates none() { return NONE; }

    public static Rates of(Rate rate) {
        return new Rates(rate);
    }

    public static Rates empty() { return new Rates(); }

    public static Rates of(Rates rates) {
        return new Rates(rates);
    }

    public static Rates or(Rate... rates) {
        return of(Operator.OR, rates);
    }

    public static Rates and(Rate... rates) { return of(Operator.AND, rates); }

    public static Rates of(Operator operator, Rate... rates) {
        return of(operator, "", rates);
    }

    public static Rates of(String rateCondition, Rate... rates) {
        return of(Operator.NONE, rateCondition, rates);
    }

    public static Rates of(String rateCondition) {
        final Rate rate = StringUtils.hasText(rateCondition)
                ? Rate.of(rateCondition) : null;
        return of(rate);
    }

    public static Rates of(Operator operator, String rateCondition, Rate... rates) {
        List<Rate> list = rates == null || rates.length == 0
                ? Collections.emptyList() : Arrays.asList(rates);
        return of(operator, rateCondition, list);
    }

    public static Rates of(List<Rate> rates) {
        return of(Operator.NONE, "", rates);
    }

    public static Rates of(Operator operator, String rateCondition, List<Rate> rates) {
        final Rate rate = StringUtils.hasText(rateCondition)
                ? Rate.of(rateCondition) : null;
        return new Rates(operator, rate, rates);
    }

    private Operator operator = Operator.NONE;

    /**
     * Multiple limits. Either set this or {@link #limit} but not both.
     * @see #limit
     */
    // !!! DO NOT ARBITRARILY RENAME !!!
    // The naming of this variable is part of this class'  contract.
    // Always access this through its getter. A small inconvenience
    // to pay for an additional single limit field.
    //
    private List<Rate> limits = Collections.emptyList();

    /**
     * A single limit. Added for convenience. Either set this or {@link #limits} but not both.
     * @see #limits
     */
    // The naming of this variable is part of this class'  contract. Do not arbitrarily rename
    //
    private Rate limit;

    // A public no-argument constructor is required
    public Rates() { }

    protected Rates(Rate limit) {
        this.limit = limit;
    }

    protected Rates(Rates rates) {
        this(rates.operator, rates.limit, rates.limits);

    }

    protected Rates(Operator operator, Rate limit, List<Rate> limits) {
        this.operator = Objects.requireNonNull(operator);
        this.limit = limit;
        this.limits = limits == null || limits.isEmpty()
                ? Collections.emptyList() : limits.stream()
                .filter(Objects::nonNull)
                .filter(Rate::isSet)
                .map(Rate::new).collect(Collectors.toList());
    }

    public boolean isSet() {
        if (limit != null && limit.isSet()) {
            return true;
        }
        for(Rate rate : limits) {
            if (rate.isSet()) {
                return true;
            }
        }
        return false;
    }

    public boolean hasSubConditions() {
        for(Rate rate : limits) {
            String condition = rate.getRateCondition();
            if (StringUtils.hasText(condition)) {
                return true;
            }
        }
        return false;
    }

    public boolean hasLimitsSet() {
        final int mainSize = limit == null || !limit.isSet() ? 0 : 1;
        if (mainSize > 0) {
            return true;
        }
        return hasSubLimitsSet();
    }

    public boolean hasSubLimitsSet() {
        if (limits == null || limits.isEmpty()) {
            return false;
        }
        if (limits.size() == 1) {
            return limits.get(0).isSet();
        }
        return limits.stream().anyMatch(Rate::isSet);
    }

    public int subLimitSize() {
        return limits == null ? 0 : limits.size();
    }

    public int totalSize() {
        final int mainSize = limit == null ? 0 : 1;
        return mainSize + subLimitSize();
    }

    public Rates limit(Rate limit) {
        setLimit(limit);
        return this;
    }

    public Rate getLimit() {
        return limit;
    }

    public void setLimit(Rate limit) {
        this.limit = limit;
    }

    public Rates operator(Operator operator) {
        setOperator(operator);
        return this;
    }

    public Operator getOperator() {
        return operator;
    }

    public void setOperator(Operator operator) {
        this.operator = operator;
    }

    public Rates limits(Rate... limits) {
        setLimits(Arrays.asList(limits));
        return this;
    }

    public Rates limits(List<Rate> limits) {
        setLimits(limits);
        return this;
    }

    public List<Rate> getSubLimits() {
        return getLimits();
    }

    public List<Rate> getLimits() {
        return limits;
    }

    public List<Rate> getAllLimits() {
        Set<Rate> result = new LinkedHashSet<>();
        // In springframework, the single limit was added twice to the limits array.
        // To prevent this, we check if the limits array already contains the single limit.
        if (limit != null) {
            result.add(limit);
        }
        if (limits != null) {
            result.addAll(limits);
        }
        return Arrays.asList(result.toArray(new Rate[0]));
    }

    public void setLimits(List<Rate> limits) {
        this.limits = limits;
    }

    // Rate related properties
    //

    public String getRateCondition() {
        return this.limit == null ? null : this.limit.getRateCondition();
    }

    public void setRateCondition(String rateCondition) {
        if (this.limit == null) {
            this.limit = Rate.of(rateCondition);
            return;
        }
        this.limit.setRateCondition(rateCondition);
    }

    public long getPermits() {
        return limit == null ? 0 : limit.getPermits();
    }

    public void setPermits(long permits) {
        if (limit == null) {
            limit = Rate.of(permits, Duration.ZERO);
            return;
        }
        limit.setPermits(permits);
    }

    public Duration getDuration() {
        return limit == null ? null : limit.getDuration();
    }

    public void setDuration(Duration duration) {
        if(limit == null) {
            limit = Rate.of(0, duration);
            return;
        }
        limit.setDuration(duration);
    }

    public Class<? extends BandwidthFactory> getFactoryClass() {
        return limit == null ? null : limit.getFactoryClass();
    }

    public void setFactoryClass(Class<? extends BandwidthFactory> factoryClass) {
        if(limit == null) {
            limit = Rate.of(0, Duration.ZERO, "", factoryClass);
            return;
        }
        limit.setFactoryClass(factoryClass);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof Rates)) {
            return false;
        }
        Rates rates = (Rates) o;
        return operator == rates.operator
                && Objects.equals(limit, rates.limit)
                && Objects.equals(limits, rates.limits);
    }

    @Override
    public int hashCode() {
        return Objects.hash(operator, limit, limits);
    }

    @Override
    public String toString() {
        return "Rates{main=" + limit + ", operator=" + operator + ", sub=" + limits + '}';
    }

    private static final Rates NONE = new Rates(){
        @Override public boolean isSet() { return false; }
        @Override public boolean hasSubConditions() { return false; }
        @Override public boolean hasLimitsSet() { return false; }
        @Override public int subLimitSize() { return 0; }
        @Override public int totalSize() { return 0; }
        @Override public void setLimit(Rate limit) { throw new UnsupportedOperationException(); }
        @Override public void setOperator(Operator optr) { throw new UnsupportedOperationException(); }
        @Override public void setLimits(List<Rate> limits) { throw new UnsupportedOperationException(); }
        @Override public void setRateCondition(String val) { throw new UnsupportedOperationException(); }
        @Override public void setPermits(long permits) { throw new UnsupportedOperationException(); }
    };
}
