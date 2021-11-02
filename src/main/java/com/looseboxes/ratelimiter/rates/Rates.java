package com.looseboxes.ratelimiter.rates;

public final class Rates {

    /**
     * Compose a new Rate comprising of the specified Rates
     *
     * All operations on the returned instance will affect all members of the composition
     * @param rates The members of the composition
     * @return A Rate composed of the specified members
     */
    public static Rate and(Rate... rates) {
        return new RateComposite(RateComposite.Logic.AND, rates);
    }

    /**
     * Compose a new Rate comprising of the specified Rates
     *
     * All operations on the returned instance will affect all members of the composition
     * @param rates The members of the composition
     * @return A Rate composed of the specified members
     */
    public static Rate or(Rate... rates) {
        return new RateComposite(RateComposite.Logic.OR, rates);
    }
}
