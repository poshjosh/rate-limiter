package com.looseboxes.ratelimiter.rates;

public interface Limit {

    Limit EMPTY_OR = Limit.of(Logic.OR);
    Limit EMPTY_AND = Limit.of(Logic.AND);

    static Limit empty(Logic logic) {
        return Logic.AND.equals(logic) ? EMPTY_AND : EMPTY_OR;
    }

    static Limit of(Rate... rates) {
        return of(Logic.OR, rates);
    }

    static Limit of(Logic logic, Rate... rates) {
        return new SimpleLimit(rates, logic);
    }

    default boolean isExceeded(int numberOfHits) {
        Logic logic = getLogic();
        return (Logic.OR.equals(logic) && numberOfHits > 0)
                || (Logic.AND.equals(logic) && numberOfHits >= getRateCount());
    }

    default boolean hasLimits() { return getRateCount() > 0; }

    default int getRateCount() {
        return getRates().length;
    }

    /**
     * Compare this Limit to a {@link Rate}.
     *
     * @param rate The Rate to compare this Limit to
     * @return
     * <p><b>The return value represents the following:</b></p>
     * <ul>
     *     <li>+1 = HAS EXCEEDED LIMIT</li>
     *     <li>0 = IS AT A THRESHOLD (Should be reset)</li>
     *     <li>-1 = IS WITHIN LIMIT</li>
     * </ul>
     * @see Rate#compareTo(Rate)
     */
    int compareTo(Rate rate);
    Logic getLogic();
    Rate[] getRates();
}
