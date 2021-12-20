package com.looseboxes.ratelimiter.annotation;

import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import java.util.Collections;
import java.util.Set;

@SupportedAnnotationTypes(RateLimitProcessor.ANNOTATION_CLASS_NAME)
@SupportedSourceVersion(SourceVersion.RELEASE_8)
public class RateLimitProcessor extends AbstractProcessor {

    public static final String ANNOTATION_CLASS_NAME = "com.looseboxes.ratelimiter.annotation.RateLimit";

    private Messager messager;

    @Override
    public synchronized void init(ProcessingEnvironment processingEnv) {
        super.init(processingEnv);
        messager = processingEnv.getMessager();
    }

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {

        annotations.forEach(annotation -> {

            Set<? extends Element> annotatedElements
                    = roundEnv.getElementsAnnotatedWith(annotation);

            annotatedElements.forEach(annotatedElement ->{

                RateLimit rateLimit = annotatedElement.getAnnotation(RateLimit.class);

                final String limitError = getErrorMessageIfInvalidLimit(rateLimit.limit(), null);
                if (limitError != null){
                    messager.printMessage(Diagnostic.Kind.ERROR, limitError);
                }

                final String durationError = getErrorMessageIfInvalidDuration(rateLimit.duration(), null);
                if(durationError != null) {
                    messager.printMessage(Diagnostic.Kind.ERROR, durationError);
                }
            });
        });

        return false;
    }

    public static String getErrorMessageIfInvalidLimit(long limit, String resultIfNone) {
        return limit < 0 ? "Invalid limit: " + limit : resultIfNone;
    }

    public static String getErrorMessageIfInvalidDuration(long duration, String resultIfNone) {
        return duration < 0 ? "Invalid duration: " + duration : resultIfNone;
    }

    @Override
    public Set<String> getSupportedAnnotationTypes() {
        return Collections.singleton(RateLimitProcessor.ANNOTATION_CLASS_NAME);
    }

    @Override
    public SourceVersion getSupportedSourceVersion() {
        return SourceVersion.RELEASE_0;
    }
}
