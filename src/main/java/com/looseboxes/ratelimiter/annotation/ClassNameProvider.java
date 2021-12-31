package com.looseboxes.ratelimiter.annotation;

public final class ClassNameProvider<E> implements IdProvider<Class<E>, String>{
    @Override public String getId(Class<E> aClass) {
        return aClass.getName();
    }
}
