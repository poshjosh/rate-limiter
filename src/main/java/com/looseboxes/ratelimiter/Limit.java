package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Logic;
import com.looseboxes.ratelimiter.rates.Rate;

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

    default boolean isExceeded(int failCount) {
        Logic logic = getLogic();
        return (Logic.OR.equals(logic) && failCount > 0)
                || (Logic.AND.equals(logic) && failCount >= getRateCount());
    }

    default boolean hasLimits() { return getRateCount() > 0; }

    default int getRateCount() {
        return getRates().length;
    }

    Logic getLogic();
    Rate[] getRates();
}
