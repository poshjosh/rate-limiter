package com.looseboxes.ratelimiter.rates;

public class FindoutBestImplementionOfCompositeRate {

    // @TODO
    @org.junit.jupiter.api.Test
    public void test() {

        LimitWithinDuration a = new LimitWithinDuration(1, 2000);
        LimitWithinDuration b = new LimitWithinDuration(2, 1000);

        LimitWithinDuration or = new LimitWithinDuration(2, 2000);
        LimitWithinDuration and = new LimitWithinDuration(1, 1000);

        for(int d = 0; d < 3000; d += 500) {

            for(int l = 0; l < 4; l ++) {

                LimitWithinDuration x = new LimitWithinDuration(l, d);

                int m = x.compareTo(or);
                if(x.compareTo(a) != m && x.compareTo(b) != m) {
                    System.out.println("ERR - " + x);
                }else{
                    System.out.println("GUT - " + x);
                }

                int n = x.compareTo(and);
                if(x.compareTo(a) != n || x.compareTo(b) != n) {
                    System.out.println("ERR - " + x);
                }else{
                    System.out.println("GUT - " + x);
                }
            }
        }
    }
}
