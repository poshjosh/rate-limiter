package com.looseboxes.ratelimiter.rates;

import java.util.Arrays;

public final class Rates {

    public enum Logic{AND, OR}

    /**
     * Compose a new Rate comprising of the specified Rates
     *
     * All operations on the returned instance will affect all members of the composition
     * @param rates The members of the composition
     * @return A Rate composed of the specified members
     */
    public static CompositeRate and(Rate... rates) {
        return compose(Rates.Logic.AND, rates);
    }

    /**
     * Compose a new Rate comprising of the specified Rates
     *
     * All operations on the returned instance will affect all members of the composition
     * @param rates The members of the composition
     * @return A Rate composed of the specified members
     */
    public static CompositeRate or(Rate... rates) {
        return compose(Rates.Logic.OR, rates);
    }

    public static CompositeRate compose(Rates.Logic logic, Rate... rates) {
        switch(logic) {
            case AND: return new AndRates(rates);
            case OR: return new OrRates(rates);
            default: throw new IllegalArgumentException("Unexpected logic: " + logic +
                    ", valid values: " + Arrays.toString(Rates.Logic.values()));
        }
    }
}
