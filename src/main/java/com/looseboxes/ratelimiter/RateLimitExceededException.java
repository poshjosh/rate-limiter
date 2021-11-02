package com.looseboxes.ratelimiter;

public class RateLimitExceededException extends RuntimeException{

    public RateLimitExceededException() { }

    public RateLimitExceededException(String message) {
        super(message);
    }
}

