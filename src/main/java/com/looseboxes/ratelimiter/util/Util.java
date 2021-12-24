package com.looseboxes.ratelimiter.util;

import com.looseboxes.ratelimiter.rates.Logic;
import com.looseboxes.ratelimiter.rates.Rate;

import java.util.concurrent.atomic.AtomicLong;

public final class Util {

    private Util() {}

    private static final AtomicLong nonce = new AtomicLong();

    public static String randomUniqueId(String prefix, String suffix) {
        synchronized (nonce) {
            return prefix + Long.toHexString(System.currentTimeMillis()) + '-' + nonce.incrementAndGet() + suffix;
        }
    }

    public static boolean isLimitExceeded(int count, Logic logic, Rate [] limits) {
        return (Logic.OR.equals(logic) && count > 0) || (Logic.AND.equals(logic) && count == limits.length);
    }
}
