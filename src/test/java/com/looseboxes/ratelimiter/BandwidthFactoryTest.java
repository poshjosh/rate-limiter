package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.bandwidths.SmoothBandwidth;
import com.looseboxes.ratelimiter.util.SleepingTicker;
import org.junit.jupiter.api.Test;

public class BandwidthFactoryTest {
    //@Test // Not really a test, just checking some equality stuff
    void test() {
        SleepingTicker ticker = SleepingTicker.zeroOffset();
        double permitsPerSec = 3;

        final long nowMicros = ticker.elapsedMicros();

        System.out.println("Now micros: " + nowMicros);

        BandwidthFactory burstyFactory = BandwidthFactory.bursty();
        Bandwidth bursty = SmoothBandwidth.bursty(permitsPerSec, nowMicros);

        Bandwidth warmingUp = SmoothBandwidth.warmingUp(permitsPerSec, nowMicros);
        BandwidthFactory warmingUpFactory = BandwidthFactory.warmingUp();

        ticker.sleepMicrosUninterruptibly(500_000);

        testCopy(burstyFactory, bursty, ticker);
        //testCopy(warmingUpFactory, warmingUp, ticker);

        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 3; j++) {
                bursty.reserveNextAvailable(1, ticker.elapsedMicros());
            }
            ticker.sleepMicrosUninterruptibly(1_000_000);
        }

        testCopy(burstyFactory, bursty, ticker);
        //testCopy(warmingUpFactory, warmingUp, ticker);
    }

    private void testCopy(BandwidthFactory bandwidthFactory, Bandwidth bandwidth, SleepingTicker ticker) {
        System.out.println();
        Bandwidth syncedCopy = bandwidth.copy(ticker.elapsedMicros());
        System.out.println(" Synced copy: " + syncedCopy);
        Bandwidth zeroedCopy = bandwidth.copy(0);
        System.out.println(" Zeroed copy: " + zeroedCopy);
        Bandwidth factoryCopy = bandwidthFactory.createNew(bandwidth.getRate());
        System.out.println("Factory copy: " + factoryCopy);
    }
}
