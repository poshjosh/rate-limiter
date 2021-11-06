package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;

import java.util.Objects;

@FunctionalInterface
public interface RateExceededHandler{

    RateExceededHandler NO_OP = (key, rate, limit) -> {};

    /**
     * Called when a rate is exceeded
     *
     * @param key The key
     * @param rate The Rate which exceeded the limit
     * @param limit The limit that was exceeded
     */
    void onRateExceeded(Object key, Rate rate, Rate limit);

    /**
     * Returns a composed {@code RateExceededHandler} that performs, in sequence, this
     * operation followed by the {@code after} operation. If performing either
     * operation throws an exception, it is relayed to the caller of the
     * composed operation.  If performing this operation throws an exception,
     * the {@code after} operation will not be performed.
     *
     * @param after the operation to perform after this operation
     * @return a composed {@code RateExceededHandler} that performs in sequence this
     * operation followed by the {@code after} operation
     * @throws NullPointerException if {@code after} is null
     */
    default RateExceededHandler andThen(RateExceededHandler after) {
        Objects.requireNonNull(after);

        return (key, rate, limit) -> {
            onRateExceeded(key, rate, limit);
            after.onRateExceeded(key, rate, limit);
        };
    }
}
