package com.looseboxes.ratelimiter.bucket4j;

import com.looseboxes.ratelimiter.Rate;
import io.github.bucket4j.Bandwidth;
import io.github.bucket4j.Bucket4j;
import io.github.bucket4j.BucketConfiguration;

public interface BucketConfigurationProvider {
    final class SimpleBucketConfigurationProvider implements BucketConfigurationProvider {
        @Override
        public BucketConfiguration getBucketConfiguration(Rate rate) {
            return Bucket4j.configurationBuilder()
                    .addLimit(Bandwidth.simple(rate.getAmount(), rate.getDuration())).build();
        }
    }

    static BucketConfigurationProvider simple() {
        return new SimpleBucketConfigurationProvider();
    }

    <R extends Rate> BucketConfiguration getBucketConfiguration(R rate);
}
