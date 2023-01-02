package com.looseboxes.ratelimiter;

public interface KeyProvider<S, K> {

    KeyProvider<Object, Object> IDENTITY = target -> target;

    @SuppressWarnings("unchecked")
    static <S, K> KeyProvider<S, K> identity() {
        return (KeyProvider<S, K>)IDENTITY;
    }

    K get(S source);
}
