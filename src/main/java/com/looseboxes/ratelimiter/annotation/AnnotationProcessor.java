package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.annotations.RateLimit;
import com.looseboxes.ratelimiter.annotations.RateLimitGroup;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.annotations.Nullable;
import com.looseboxes.ratelimiter.util.Operator;
import com.looseboxes.ratelimiter.util.Rates;

import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.Method;
import java.util.List;
import java.util.function.BiConsumer;

public interface AnnotationProcessor<S extends GenericDeclaration, T> {

    Operator DEFAULT_OPERATOR = Operator.OR;

    interface Converter<T>{
        T convert(RateLimitGroup rateLimitGroup, RateLimit [] rateLimits);
        boolean isOperatorEqual(T type, Operator operator);
    }

    static AnnotationProcessor<Class<?>, Rates> ofRates() {
        return ofRates(IdProvider.ofClass(), IdProvider.ofMethod());
    }

    static AnnotationProcessor<Class<?>, Rates> ofRates(
            IdProvider<Class<?>, String> idProviderForClass, IdProvider<Method, String> idProviderForMethod) {
        return of(idProviderForClass, idProviderForMethod, new AnnotationToRatesConverter());
    }

    static <T> AnnotationProcessor<Class<?>, T> of(
            IdProvider<Class<?>, String> idProviderForClass,
            IdProvider<Method, String> idProviderForMethod,
            Converter<T> converter) {
        return new ClassAnnotationProcessor<>(idProviderForClass, converter,
                new MethodAnnotationProcessor<>(idProviderForMethod, converter));
    }

    default void process(Node<NodeData<T>> root, List<S> elements) {
        process(root, elements, (element, node) -> {});
    }

    default void process(@Nullable Node<NodeData<T>> root, List<S> elements, BiConsumer<Object, Node<NodeData<T>>> consumer) {
        elements.forEach(clazz -> process(root, clazz, consumer));
    }

    Node<NodeData<T>> process(@Nullable Node<NodeData<T>> root, S element, BiConsumer<Object, Node<NodeData<T>>> consumer);
}
