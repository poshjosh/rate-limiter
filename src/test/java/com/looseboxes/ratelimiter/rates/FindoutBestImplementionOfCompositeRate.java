package com.looseboxes.ratelimiter.rates;

public class FindoutBestImplementionOfCompositeRate {

    // @TODO
    @org.junit.jupiter.api.Test
    public void test() {

        AmountPerDuration a = AmountPerDuration.of(1, 2000);
        AmountPerDuration b = AmountPerDuration.of(2, 1000);

        AmountPerDuration or = AmountPerDuration.of(2, 2000);
        AmountPerDuration and = AmountPerDuration.of(1, 1000);

        for(int d = 0; d < 3000; d += 500) {

            for(int l = 0; l < 4; l ++) {

                AmountPerDuration x = AmountPerDuration.of(l, d);

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
