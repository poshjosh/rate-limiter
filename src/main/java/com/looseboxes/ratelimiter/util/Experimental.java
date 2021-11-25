package com.looseboxes.ratelimiter.util;

import java.lang.annotation.*;

/**
 * Annotation that signifies that an element is experimental, and may change without notice.
 */
@Documented
@Retention(value=RetentionPolicy.CLASS)
@Target({ElementType.TYPE, ElementType.FIELD, ElementType.METHOD,
        ElementType.CONSTRUCTOR, ElementType.LOCAL_VARIABLE, ElementType.ANNOTATION_TYPE
})
public @interface Experimental {
}
