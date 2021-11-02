package com.looseboxes.ratelimiter.util;

import java.util.List;

@FunctionalInterface
public interface ClassesInPackageFinder {
    List<Class<?>> findClasses(String packageName, ClassFilter classFilter);
}
