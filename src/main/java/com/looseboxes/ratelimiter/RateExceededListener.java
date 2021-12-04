package com.looseboxes.ratelimiter;

import java.util.Objects;

public interface RateExceededListener {

    RateExceededListener NO_OP = rateExceededEvent -> { };

    /**
     * Called when a rate is recorded
     *
     * @param rateExceededEvent The event
     */
    void onRateExceeded(RateExceededEvent rateExceededEvent);

    /**
     * Returns a composed {@code RateExceededListener} that performs, in sequence, this
     * operation followed by the {@code after} operation. If performing either
     * operation throws an exception, it is relayed to the caller of the
     * composed operation.  If performing this operation throws an exception,
     * the {@code after} operation will not be performed.
     *
     * @param after the operation to perform after this operation
     * @return a composed {@code RateExceededListener} that performs in sequence this
     * operation followed by the {@code after} operation
     * @throws NullPointerException if {@code after} is null
     */
    default RateExceededListener andThen(RateExceededListener after) {
        Objects.requireNonNull(after);
        return rateExceededEvent -> {
            RateExceededListener.this.onRateExceeded(rateExceededEvent);
            after.onRateExceeded(rateExceededEvent);
        };
    }
}
