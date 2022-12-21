package com.looseboxes.ratelimiter.bucket4j;

import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import io.github.bucket4j.Bucket4j;
import io.github.bucket4j.BucketConfiguration;

import java.time.Duration;

public interface BucketConfigurationProvider {
    final class SimpleBucketConfigurationProvider implements BucketConfigurationProvider {
        @Override
        public BucketConfiguration getBucketConfiguration(Bandwidth bandwidth) {
            final long permitsPerNanos = bandwidth.getPermitsPerNanos();
            return Bucket4j.configurationBuilder()
                    .addLimit(io.github.bucket4j.Bandwidth.simple(permitsPerNanos, Duration.ofNanos(1))).build();
        }
    }

    static BucketConfigurationProvider simple() {
        return new SimpleBucketConfigurationProvider();
    }

    <R extends Bandwidth> BucketConfiguration getBucketConfiguration(R rate);
}
