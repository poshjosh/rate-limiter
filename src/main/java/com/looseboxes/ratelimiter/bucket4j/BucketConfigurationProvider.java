package com.looseboxes.ratelimiter.bucket4j;

import com.looseboxes.ratelimiter.rates.AmountPerDuration;
import com.looseboxes.ratelimiter.rates.Rate;
import io.github.bucket4j.Bandwidth;
import io.github.bucket4j.Bucket4j;
import io.github.bucket4j.BucketConfiguration;

import java.time.Duration;

public interface BucketConfigurationProvider {
    final class BucketConfigurationProviderImpl implements BucketConfigurationProvider {
        @Override
        public BucketConfiguration getBucketConfiguration(Rate rate) {
            // @TODO This cast is a code smell - redesign
            final AmountPerDuration amountPerDuration = (AmountPerDuration) rate;
            return Bucket4j.configurationBuilder().addLimit(Bandwidth
                    .simple(amountPerDuration.getAmount(),
                            Duration.ofMillis(amountPerDuration.getDuration()))).build();
        }
    }

    static BucketConfigurationProvider simple() {
        return new BucketConfigurationProviderImpl();
    }

    <R extends Rate> BucketConfiguration getBucketConfiguration(R rate);
}
