So that we can have methods without resourceId, we will introduce a default global key
   Make sure the single argument method, has synced argument between RateLimiterIx and RateLimiter
Let RateLimiter2 implement our original RateLimiter, then test that exhaustively

Remove all System.out.println and any print() methods
From RateLimiterTest move all Rate related tests to own class 
RateLimiter2.setRate should have package access and visible for testing.
Implement serialization properly (esp for cache)
Implement VisibleForTesting
Remove all @since
Remove RateLimiter.setRate and add the rate to the constructor of RateLimiter