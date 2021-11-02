package com.looseboxes.ratelimiter.annotation;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.annotation.processing.SupportedSourceVersion;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import java.util.Set;

@SupportedAnnotationTypes("com.looseboxes.ratelimiter.annotation.RateLimit")
@SupportedSourceVersion(SourceVersion.RELEASE_8)
public class RateLimitProcessor extends AbstractProcessor {

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {

        annotations.forEach(annotation -> {

            Set<? extends Element> annotatedElements
                    = roundEnv.getElementsAnnotatedWith(annotation);

            annotatedElements.forEach(annotatedElement ->{

                RateLimit rateLimit = annotatedElement.getAnnotation(RateLimit.class);

                final String limitError = getErrorMessageIfInvalidLimit(rateLimit.limit(), null);
                if (limitError != null){
                    processingEnv.getMessager().printMessage(Diagnostic.Kind.ERROR, limitError);
                }

                final String durationError = getErrorMessageIfInvalidDuration(rateLimit.duration(), null);
                if(durationError != null) {
                    processingEnv.getMessager().printMessage(Diagnostic.Kind.ERROR, durationError);
                }
            });
        });

        return false;
    }

    public static String getErrorMessageIfInvalidLimit(int limit, String resultIfNone) {
        return limit < 0 ? "Invalid limit: " + limit : resultIfNone;
    }

    public static String getErrorMessageIfInvalidDuration(long duration, String resultIfNone) {
        return duration < 0 ? "Invalid duration: " + duration : resultIfNone;
    }
}
