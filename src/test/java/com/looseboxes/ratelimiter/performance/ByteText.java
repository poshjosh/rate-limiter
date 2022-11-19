package com.looseboxes.ratelimiter.performance;

public final class ByteText {
    private ByteText() { }
    public static String of(long amount) {
        final String sign = amount < 0 ? "-" : "";
        amount = Math.abs(amount);
        final int oneGB = 1_000_000_000;
        if (amount >= oneGB) {
            return sign + (amount / oneGB) + " GB";
        }
        final int oneMB = oneGB / 1000;
        if (amount >= oneMB) {
            return sign + (amount / oneMB) + " MB";
        }
        final int oneKB = oneMB / 1000;
        if (amount >= oneKB) {
            return sign + (amount / oneKB) + " KB";
        }
        return  sign + amount + " bytes";
    }
}
