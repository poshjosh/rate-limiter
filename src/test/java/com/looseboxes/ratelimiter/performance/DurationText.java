package com.looseboxes.ratelimiter.performance;

public final class DurationText {
    private DurationText() { }
    public static String of(long time) {
        final String sign = time < 0 ? "-" : "";
        time = Math.abs(time);
        final int oneHr = 60 * 60 * 1000;
        if (time >= oneHr) {
            return sign + (time / oneHr) + " hr";
        }
        final int oneMin = oneHr / 60;
        if (time >= oneMin) {
            return sign + (time / oneMin) + " min";
        }
        final int oneSec = oneMin / 60;
        if (time >= oneSec) {
            return sign + (time / oneSec) + " sec";
        }
        return  sign + time + " millis";
    }
}
