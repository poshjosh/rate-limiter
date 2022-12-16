Should RateLimiter have method setRate() ? What about getRate()?. 
  We think setRate may be an overkill. However, many tests use that method.

SleepingStopwatch should not extend Stopwatch

Method sleepUninterruptibly does not belong.
For method readMicros, we should simply use Stopwatch.elapsed