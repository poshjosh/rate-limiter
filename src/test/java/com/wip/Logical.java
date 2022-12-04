package com.wip;

import com.looseboxes.ratelimiter.rates.Logic;

public interface Logical<M extends Logical> {
    M compose(Logic logic, M... members);
}
