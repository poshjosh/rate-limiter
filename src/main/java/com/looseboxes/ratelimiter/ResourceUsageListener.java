package com.looseboxes.ratelimiter;

import java.util.Objects;

public interface ResourceUsageListener {

    ResourceUsageListener NO_OP = new ResourceUsageListener() { };

    default void onConsumed(Object context, Object resourceId, int hits, Object limit) { }

    default void onRejected(Object context, Object resourceId, int hits, Object limit) { }

    /**
     * Returns a composed {@code ResourceUsageListener} that performs, in sequence, this
     * operation followed by the {@code after} operation. If performing either
     * operation throws an exception, it is relayed to the caller of the
     * composed operation.  If performing this operation throws an exception,
     * the {@code after} operation will not be performed.
     *
     * @param after the operation to perform after this operation
     * @return a composed {@code ResourceUsageListener} that performs in sequence this
     * operation followed by the {@code after} operation
     * @throws NullPointerException if {@code after} is null
     */
    default ResourceUsageListener andThen(ResourceUsageListener after) {
        Objects.requireNonNull(after);
        return new ResourceUsageListener() {
            @Override
            public void onConsumed(Object context, Object resourceId, int hits, Object limit) {
                ResourceUsageListener.this.onConsumed(context, resourceId, hits, limit);
                after.onConsumed(context, resourceId, hits, limit);
            }
            @Override
            public void onRejected(Object context, Object resourceId, int hits, Object limit) {
                ResourceUsageListener.this.onRejected(context, resourceId, hits, limit);
                after.onRejected(context, resourceId, hits, limit);
            }
        };
    }
}
