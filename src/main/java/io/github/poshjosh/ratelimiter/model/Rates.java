package io.github.poshjosh.ratelimiter.model;

import java.time.Duration;
import java.util.*;
import java.util.stream.Collectors;

public class Rates implements java.io.Serializable {

    private static final long serialVersionUID = 110L;

    public static Rates none() { return NONE; }

    public static Rates of(Rate rate) {
        return new Rates(rate);
    }

    public static Rates ofId(String id) {
        return of(id, (Rate)null);
    }

    public static Rates of(String id, Rate rate) {
        return new Rates(id, null, rate, null);
    }

    public static Rates empty() { return new Rates(); }

    public static Rates or(Rate... rates) {
        return of(Operator.OR, rates);
    }

    public static Rates and(Rate... rates) { return of(Operator.AND, rates); }

    public static Rates of(Operator operator, Rate... rates) {
        return of(operator, null, rates);
    }

    public static Rates of(String condition, Rate... rates) {
        return of(null, condition, rates);
    }

    public static Rates ofCondition(String condition) {
        return of(condition == null || condition.isEmpty() ? null : Rate.ofCondition(condition));
    }

    public static Rates of(Operator operator, String condition, Rate... rates) {
        return of(null, operator, condition, rates);
    }

    public static Rates of(String id, Operator operator, String condition, Rate... rates) {
        List<Rate> list = rates == null || rates.length == 0
                ? Collections.emptyList() : Arrays.asList(rates);
        return of(id, operator, condition, list);
    }

    public static Rates of(List<Rate> rates) {
        return of(null, null, rates);
    }

    public static Rates of(Operator operator, String condition, List<Rate> rates) {
        return of(null, operator, condition, rates);
    }

    public static Rates of(String id, Operator operator, String condition, List<Rate> rates) {
        final Rate rate = condition == null || condition.isEmpty() ? null : Rate.ofCondition(condition);
        return new Rates(id, operator, rate, rates);
    }

    private static String randomId() {
        return UUID.randomUUID().toString();
    }

    private String parentId;

    private String id;

    private Operator operator;

    /**
     * Multiple limits. Either set this or {@link #limit} but not both.
     * @see #limit
     */
    // !!! DO NOT ARBITRARILY RENAME !!!
    // The naming of this variable is part of this class'  contract.
    // Always access this through its getter. A small inconvenience
    // to pay for an additional single limit field.
    //
    private List<Rate> limits;

    /**
     * A single limit. Added for convenience. Either set this or {@link #limits} but not both.
     * @see #limits
     */
    // The naming of this variable is part of this class'  contract. Do not arbitrarily rename
    //
    private Rate limit;

    // A public no-argument constructor is required
    public Rates() {
        this(null);
    }

    protected Rates(Rate limit) {
        this(null, null, limit, null);
    }

    protected Rates(String id, Operator operator, Rate limit, List<Rate> limits) {
        this.id = id == null ? randomId() : id;
        this.operator = operator == null ? Operator.NONE : operator;
        this.limit = limit;
        this.limits = limits == null || limits.isEmpty()
                ? Collections.emptyList() : limits.stream()
                .filter(Objects::nonNull)
                .filter(Rate::isSet)
                .map(Rate::new).collect(Collectors.toList());
    }

    public Rates copy() {
        return new Rates(id, operator, limit, limits);
    }

    public boolean isSet() {
        if (limit != null && limit.isSet()) {
            return true;
        }
        if (limits == null) {
            return false;
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
            String condition = rate.getCondition();
            if (condition != null && !condition.isEmpty()) {
                return true;
            }
        }
        return false;
    }

    public int subLimitSize() {
        return limits == null ? 0 : limits.size();
    }

    public int totalSize() {
        final int mainSize = limit == null ? 0 : 1;
        return mainSize + subLimitSize();
    }

    public Rates parentId(String parentId) {
        setParentId(parentId);
        return this;
    }

    public String getParentId() {
        return this.parentId;
    }

    public void setParentId(String parentId) {
        this.parentId = parentId;
    }

    public Rates id(String id) {
        setId(id);
        return this;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = Objects.requireNonNull(id);
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
        return this.limit == null ? null : this.limit.getCondition();
    }

    public void setRateCondition(String condition) {
        if (this.limit == null) {
            this.limit = Rate.ofCondition(condition);
            return;
        }
        this.limit.setCondition(condition);
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

    public String getFactoryClass() {
        return limit == null ? null : limit.getFactoryClass();
    }

    public void setFactoryClass(String factoryClass) {
        if(limit == null) {
            limit = Rate.of(0, Duration.ZERO, "", factoryClass);
            return;
        }
        limit.setFactoryClass(factoryClass);
    }

    @Override public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;

        Rates rates = (Rates) o;

        return id.equals(rates.id);
    }

    @Override public int hashCode() {
        return id.hashCode();
    }

    @Override
    public String toString() {
        return "Rates{id=" + id + ", main=" + limit
                + ", operator=" + operator + ", sub=" + limits + '}';
    }

    private static final Rates NONE = new Rates("", null, null, null){
        @Override public boolean isSet() { return false; }
        @Override public boolean hasSubConditions() { return false; }
        @Override public int subLimitSize() { return 0; }
        @Override public int totalSize() { return 0; }
        @Override public void setId(String id) { throw new UnsupportedOperationException(); }
        @Override public void setParentId(String parentId) { throw new UnsupportedOperationException(); }
        @Override public void setLimit(Rate limit) { throw new UnsupportedOperationException(); }
        @Override public void setOperator(Operator optr) { throw new UnsupportedOperationException(); }
        @Override public void setLimits(List<Rate> limits) { throw new UnsupportedOperationException(); }
        @Override public void setRateCondition(String val) { throw new UnsupportedOperationException(); }
        @Override public void setPermits(long permits) { throw new UnsupportedOperationException(); }
    };
}
