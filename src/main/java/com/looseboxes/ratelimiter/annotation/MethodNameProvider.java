package com.looseboxes.ratelimiter.annotation;

import java.lang.reflect.Method;

public final class MethodNameProvider implements IdProvider<Method, String>{
    @Override public String getId(Method method) {
        final String methodString = method.toString();
        final int indexOfClassName = methodString.indexOf(method.getDeclaringClass().getName());
        if(indexOfClassName == -1) {
            throw new AssertionError("Method#toString() does not contain the method's declaring class name as expected. Method: " + method);
        }
        return methodString.substring(indexOfClassName);
    }
}
