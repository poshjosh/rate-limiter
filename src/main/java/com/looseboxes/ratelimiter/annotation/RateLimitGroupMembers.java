package com.looseboxes.ratelimiter.annotation;

import java.util.*;

public final class RateLimitGroupMembers<S> {

    private final String name;

    private Set<S> members;

    public RateLimitGroupMembers(String name) {
        this.name = Objects.requireNonNull(name);
    }

    public String getName() {
        return name;
    }

    public boolean addElement(S element) {
        Objects.requireNonNull(element);
        if(members == null) {
            members = new HashSet<>();
        }
        return members.add(element);
    }

    public Collection<S> getMembers() {
        return members == null || members.isEmpty() ? Collections.emptyList() : Collections.unmodifiableSet(members);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        RateLimitGroupMembers<?> that = (RateLimitGroupMembers<?>) o;
        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public String toString() {
        return "RateLimitGroupMembers{" +
                "name='" + name + '\'' +
                ", members=" + members +
                '}';
    }
}
