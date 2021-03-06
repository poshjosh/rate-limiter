package com.looseboxes.ratelimiter.util;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@FunctionalInterface
public interface ClassesInPackageFinder {

    default List<Class<?>> findClasses(List<String> packageNames, ClassFilter classFilter) {
        Objects.requireNonNull(classFilter);
        return packageNames.isEmpty() ? Collections.emptyList() : packageNames.stream()
                .flatMap(packageName -> findClasses(packageName, classFilter).stream())
                .collect(Collectors.toList());
    }

    List<Class<?>> findClasses(String packageName, ClassFilter classFilter);
}
