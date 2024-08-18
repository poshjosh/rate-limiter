package io.github.poshjosh.ratelimiter.model;

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
        return new Rates(null, id, null, null, Collections.singletonList(rate));
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
        return of(null, condition);
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
        return new Rates(null, id, operator, condition, rates);
    }

    private static String randomId() {
        return UUID.randomUUID().toString();
    }

    private String parentId;

    private String id;

    private Operator operator;

    private List<Rate> rates;

    private String condition;

    // A public no-argument constructor is required
    public Rates() {
        this((Rate)null);
    }

    protected Rates(Rate rate) {
        this(null, null, null, null, Collections.singletonList(rate));
    }

    protected Rates(Rates rates) {
        this(rates.parentId, rates.id, rates.operator, rates.condition, rates.rates);
    }

    protected Rates(
            String parentId, String id, Operator operator, String condition, List<Rate> rates) {
        this.parentId = parentId;
        this.id = id == null ? randomId() : id;
        this.operator = operator == null ? Operator.NONE : operator;
        this.condition = condition;
        this.rates = rates == null || rates.isEmpty()
                ? Collections.emptyList() : rates.stream()
                .filter(Objects::nonNull)
                .filter(Rate::isSet)
                .map(Rate::new).collect(Collectors.toList());
    }

    public Rates copy() {
        return new Rates(this);
    }

    public boolean isSet() {
        if (rates == null) {
            return false;
        }
        for(Rate rate : rates) {
            if (rate.isSet()) {
                return true;
            }
        }
        return false;
    }

    public boolean hasSubConditions() {
        for(Rate rate : rates) {
            String val = rate.getCondition();
            if (val != null && !val.isEmpty()) {
                return true;
            }
        }
        return false;
    }

    public int subRateSize() {
        return rates == null ? 0 : rates.size();
    }

    public int size() {
        return rates == null ? 0 : rates.size();
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

    public Rates rates(Rate... rates) {
        setRates(Arrays.asList(rates));
        return this;
    }

    public Rates rates(List<Rate> rates) {
        setRates(rates);
        return this;
    }

    public List<Rate> getRates() {
        return rates;
    }

    public void setRates(List<Rate> rates) {
        this.rates = rates;
    }
    public Rates condition(String condition) {
        setCondition(condition);
        return this;
    }

    public String getCondition() {
        return condition;
    }

    public void setCondition(String condition) {
        this.condition = condition;
    }

    @Override public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;

        Rates ratesObj = (Rates) o;

        return id.equals(ratesObj.id);
    }

    @Override public int hashCode() {
        return id.hashCode();
    }

    @Override
    public String toString() {
        return "Rates{parentId=" + parentId + ", id=" + id + ", operator=" + operator
                + ", condition=" + condition + ", rates=" + rates + '}';
    }

    private static final Rates NONE = new Rates(null, "", null, null, null){
        @Override public boolean isSet() { return false; }
        @Override public boolean hasSubConditions() { return false; }
        @Override public int subRateSize() { return 0; }
        @Override public int size() { return 0; }
        @Override public void setId(String id) { throw new UnsupportedOperationException(); }
        @Override public void setParentId(String parentId) { throw new UnsupportedOperationException(); }
        @Override public void setOperator(Operator optr) { throw new UnsupportedOperationException(); }
        @Override public void setRates(List<Rate> rates) { throw new UnsupportedOperationException(); }
        @Override public void setCondition(String val) { throw new UnsupportedOperationException(); }
    };
}
