package com.looseboxes.ratelimiter.util;

import java.util.Collection;

public interface RateLimitGroupData<S> {
    RateLimitConfig getConfig();
    Collection<S> getMembers();
}
