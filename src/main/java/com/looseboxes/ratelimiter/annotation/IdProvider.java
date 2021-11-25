package com.looseboxes.ratelimiter.annotation;

/**
 * Provide an id
 * @param <SOURCE> The type of the object for which an id is to be provided
 * @param <ID> The type of the id
 */
public interface IdProvider<SOURCE, ID> {
    ID getId(SOURCE source);
}
