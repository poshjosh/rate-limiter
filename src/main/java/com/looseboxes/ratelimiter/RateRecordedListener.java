package com.looseboxes.ratelimiter;

import java.util.Objects;

public interface RateRecordedListener {

    RateRecordedListener NO_OP = rateRecordedEvent -> { };

    /**
     * Called when a rate is recorded
     *
     * @param rateRecordedEvent The event
     */
    void onRateRecorded(RateRecordedEvent rateRecordedEvent);

    /**
     * Returns a composed {@code RateRecordedListener} that performs, in sequence, this
     * operation followed by the {@code after} operation. If performing either
     * operation throws an exception, it is relayed to the caller of the
     * composed operation.  If performing this operation throws an exception,
     * the {@code after} operation will not be performed.
     *
     * @param after the operation to perform after this operation
     * @return a composed {@code RateRecordedListener} that performs in sequence this
     * operation followed by the {@code after} operation
     * @throws NullPointerException if {@code after} is null
     */
    default RateRecordedListener andThen(RateRecordedListener after) {
        Objects.requireNonNull(after);
        return rateRecordedEvent -> {
            RateRecordedListener.this.onRateRecorded(rateRecordedEvent);
            after.onRateRecorded(rateRecordedEvent);
        };
    }
}
