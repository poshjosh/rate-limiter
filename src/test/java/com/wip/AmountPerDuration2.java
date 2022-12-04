package com.wip;

import com.looseboxes.ratelimiter.rates.Logic;

public class AmountPerDuration2 implements Rate2<AmountPerDuration2> {

    private final Amount amount;
    private final Amount duration;

    public AmountPerDuration2(Amount amount, Amount duration) {
        this.amount = amount;
        this.duration = duration;
    }

    @Override
    public AmountPerDuration2 compose(Logic logic, AmountPerDuration2... members) {
        // @TODO
        // This is the problem, we can't accurately compose multiple rates
        return null;
    }

    @Override
    public Amount getX() {
        return amount;
    }

    @Override
    public Amount getY() {
        return duration;
    }
}
