package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;

import java.util.Collection;
import java.util.Objects;

public interface RateRecordedListener {

    RateRecordedListener NO_OP = new RateRecordedListener() { };

    /**
     * Called when a rate is recorded
     */
    default void onRateRecorded(Object resource, Object resourceId, int hitsToRecord, Collection<Rate> exceededLimits) { }

    /**
     * Called when a rate is exceeded
     */
    default void onRateExceeded(Object resource, Object resourceId, int hitsToRecord, Collection<Rate> exceededLimits) { }

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
            public void onRateRecorded(Object resource, Object resourceId, int hitsToRecord, Collection<Rate> exceededLimits) {
                RateRecordedListener.this.onRateRecorded(resource, resourceId, hitsToRecord, exceededLimits);
                after.onRateRecorded(resource, resourceId, hitsToRecord, exceededLimits);
            }
            @Override
            public void onRateExceeded(Object resource, Object resourceId, int amount, Collection<Rate> exceededLimits) {
                RateRecordedListener.this.onRateExceeded(resource, resourceId, amount, exceededLimits);
                after.onRateExceeded(resource, resourceId, amount, exceededLimits);
            }
        };
    }
}
