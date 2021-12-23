package com.looseboxes.ratelimiter;

public class RateExceededException extends RuntimeException{

    public RateExceededException() { }

    public RateExceededException(String message) {
        super(message);
    }
}

