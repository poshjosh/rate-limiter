package com.looseboxes.ratelimiter.util;

import com.looseboxes.ratelimiter.annotation.AnnotationProcessingException;
import com.looseboxes.ratelimiter.annotation.RateLimitGroup;
import com.looseboxes.ratelimiter.rates.Logic;

import java.util.*;

public class RateLimitGroupDataImpl<S> implements RateLimitGroupData<S> {

    private RateLimitConfig rateLimitConfig;
    private Set<S> members;

    public RateLimitGroupDataImpl() { }

    @Override
    public RateLimitConfig getConfig() {
        return rateLimitConfig;
    }

    public void setRateLimitConfig(RateLimitConfig rateLimitConfig) {
        this.rateLimitConfig = rateLimitConfig;
    }

    public boolean addMember(String name, S member) {
        Objects.requireNonNull(member);
        if(members == null) {
            members = new HashSet<>();
        }
        boolean added = members.add(member);
        if(members.size() > 1) {
            throw new AnnotationProcessingException("A " + RateLimitGroup.class.getName() +
                    " may not be applied to more than 1 class/method. Group: " + name + ", applied to: " + members);
        }
        return added;
    }

    public void requireEqualLogic(RateLimitGroup rateLimitGroup) {
        Logic existingLogic = rateLimitConfig == null ? null : rateLimitConfig.getLogic();
        Logic currentLogic = rateLimitGroup.logic();
        if(existingLogic != null && !existingLogic.equals(currentLogic)) {
            throw new AnnotationProcessingException(RateLimitGroup.class.getName() +
                    " was defined more than once, with different values for logic, at: " + members);
        }
    }

    @Override
    public Collection<S> getMembers() {
        return members == null || members.isEmpty() ? Collections.emptyList() : Collections.unmodifiableSet(members);
    }
}
