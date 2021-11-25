package com.looseboxes.ratelimiter.annotation;

public final class ClassNameProvider implements IdProvider<Class<?>, String>{
    @Override public String getId(Class<?> aClass) {
        return aClass.getName();
    }
}
