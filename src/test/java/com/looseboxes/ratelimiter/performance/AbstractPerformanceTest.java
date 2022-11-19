package com.looseboxes.ratelimiter.performance;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

public class AbstractPerformanceTest {

    void assertUsageSinceBookmarkIsLessThan(Usage bookmark, Usage limit) {
        Usage usage = bookmark.usage();
        System.out.printf("Spent %s", DurationText.of(usage.getDuration()));
        assertThat(usage.getDuration()).isLessThanOrEqualTo(limit.getDuration());
        System.out.printf(", %s", ByteText.of(usage.getMemory()));
        assertThat(usage.getMemory()).isLessThanOrEqualTo(limit.getMemory());
    }
}
