package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.CompositeRate;

import java.util.Objects;

public interface RateRecordedListener {

    RateRecordedListener NO_OP = new RateRecordedListener() { };

    /**
     * Called when a rate is recorded
     */
    default void onRateRecorded(Object context, Object resourceId, int recordedHits, CompositeRate limit) { }

    /**
     * Called when a rate is exceeded
     */
    default void onRateExceeded(Object context, Object resourceId, int recordedHits, CompositeRate limit) { }

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
        return new RateRecordedListener() {
            @Override
            public void onRateRecorded(Object context, Object resourceId, int recordedHits, CompositeRate limit) {
                RateRecordedListener.this.onRateRecorded(context, resourceId, recordedHits, limit);
                after.onRateRecorded(context, resourceId, recordedHits, limit);
            }
            @Override
            public void onRateExceeded(Object context, Object resourceId, int recordedHits, CompositeRate limit) {
                RateRecordedListener.this.onRateExceeded(context, resourceId, recordedHits, limit);
                after.onRateExceeded(context, resourceId, recordedHits, limit);
            }
        };
    }
}
