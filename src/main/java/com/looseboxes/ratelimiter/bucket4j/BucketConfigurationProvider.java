package com.looseboxes.ratelimiter.bucket4j;

import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import io.github.bucket4j.Bucket4j;
import io.github.bucket4j.BucketConfiguration;

import java.time.Duration;
import java.util.concurrent.TimeUnit;

public interface BucketConfigurationProvider {
    final class SimpleBucketConfigurationProvider implements BucketConfigurationProvider {
        @Override
        public BucketConfiguration getBucketConfiguration(Bandwidth bandwidth) {
            // We convert to the largest possible, to reduce our loss of precision
            // (when casting from double to long) to the barest minimum.
            final long permitsPerDay = (long)bandwidth.getRate() * TimeUnit.SECONDS.toDays(1L);
            return Bucket4j.configurationBuilder()
                    .addLimit(io.github.bucket4j.Bandwidth.simple(permitsPerDay, Duration.ofDays(1))).build();
        }
    }

    static BucketConfigurationProvider simple() {
        return new SimpleBucketConfigurationProvider();
    }

    <R extends Bandwidth> BucketConfiguration getBucketConfiguration(R rate);
}
