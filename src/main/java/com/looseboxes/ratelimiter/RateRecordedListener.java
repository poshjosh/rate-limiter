package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;

import java.util.Objects;

public interface RateRecordedListener {

    RateRecordedListener NO_OP = new RateRecordedListener() {
        @Override
        public void onRateRecorded(Object key, Rate rate) { }
        @Override
        public void onRateExceeded(Object key, Rate rate, Rate exceededRate) { }
    };

    /**
     * Called when a rate is recorded
     *
     * @param key The key
     * @param rate The Rate which exceeded the limit
     */
    void onRateRecorded(Object key, Rate rate);

    /**
     * Called when a rate is exceeded
     *
     * @param key The key
     * @param rate The Rate which exceeded the limit
     * @param exceededRate The limit that was exceeded
     */
    void onRateExceeded(Object key, Rate rate, Rate exceededRate);

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
            public void onRateRecorded(Object key, Rate rate) {
                RateRecordedListener.this.onRateRecorded(key, rate);
                after.onRateRecorded(key, rate);
            }
            @Override
            public void onRateExceeded(Object key, Rate rate, Rate exceededRate) {
                RateRecordedListener.this.onRateExceeded(key, rate, exceededRate);
                after.onRateExceeded(key, rate, exceededRate);
            }
        };
    }
}
