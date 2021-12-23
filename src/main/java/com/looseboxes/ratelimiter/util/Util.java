package com.looseboxes.ratelimiter.util;

import java.util.concurrent.atomic.AtomicLong;

public final class Util {

    private Util() {}

    private static final AtomicLong nonce = new AtomicLong();

    public static String randomUniqueId(String prefix, String suffix) {
        synchronized (nonce) {
            return prefix + Long.toHexString(System.currentTimeMillis()) + '-' + nonce.incrementAndGet() + suffix;
        }
    }
}
