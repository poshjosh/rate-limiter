package com.wip;

public interface Amount extends Comparable<Amount>{
    long in(AmountUnit unit);
    Amount addTo(Amount amount);
    Amount subtractFrom(Amount amount);
}
