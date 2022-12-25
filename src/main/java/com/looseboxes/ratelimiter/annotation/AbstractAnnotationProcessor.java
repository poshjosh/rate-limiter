package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.annotations.Nullable;
import com.looseboxes.ratelimiter.annotations.RateLimit;
import com.looseboxes.ratelimiter.annotations.RateLimitGroup;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.formatters.NodeFormatters;
import com.looseboxes.ratelimiter.util.Operator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.GenericDeclaration;
import java.util.Objects;
import java.util.function.BiConsumer;

public abstract class AbstractAnnotationProcessor<S extends GenericDeclaration, T>
        implements AnnotationProcessor<S, T>{

    private static final Logger LOG = LoggerFactory.getLogger(AbstractAnnotationProcessor.class);

    private final IdProvider<S, String> idProvider;

    private final Converter<T> converter;

    protected AbstractAnnotationProcessor(IdProvider<S, String> idProvider, Converter<T> converter) {
        this.idProvider = Objects.requireNonNull(idProvider);
        this.converter = Objects.requireNonNull(converter);
    }

    protected abstract Node<NodeData<T>> getOrCreateParent(
            @Nullable Node<NodeData<T>> root, S element,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits);

    public Node<NodeData<T>> process(@Nullable Node<NodeData<T>> root, S element, BiConsumer<Object, Node<NodeData<T>>> consumer){

        final RateLimit [] rateLimits = element.getAnnotationsByType(RateLimit.class);

        final Node<NodeData<T>> node;

        if(rateLimits.length > 0 ) {

            RateLimitGroup rateLimitGroup = element.getAnnotation(RateLimitGroup.class);
            Node<NodeData<T>> created = getOrCreateParent(root, element, rateLimitGroup, rateLimits);

            Node<NodeData<T>> parentNode = created == null ? root : created;
            String name = idProvider.getId(element);
            node = createNodeForElementOrNull(parentNode, name, element, rateLimitGroup, rateLimits);

        }else{
            node = null;
        }
        if(LOG.isTraceEnabled()) {
            LOG.trace("\nProcessed: {}\nInto Node: {}", element, NodeFormatters.indented().format(node));
        }

        consumer.accept(element, node);

        return node;
    }

    protected Node<NodeData<T>> findOrCreateNodeForRateLimitGroupOrNull(
            @Nullable Node<NodeData<T>> root, Node<NodeData<T>> parent,
            GenericDeclaration annotatedElement, RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        String name = getName(rateLimitGroup);
        final Node<NodeData<T>> node;
        if(root == null || rateLimitGroup == null || name.isEmpty()) {
            node = null;
        }else{
            node = root.findFirstChild(n -> name.equals(n.getName()))
                    .map(foundNode -> requireConsistentData(foundNode, annotatedElement, rateLimitGroup, rateLimits))
                    .orElseGet(() -> createNodeForGroupOrNull(parent, name, rateLimitGroup, rateLimits));
        }

        return node;
    }

    private String getName(RateLimitGroup rateLimitGroup) {
        return rateLimitGroup == null ? "" : selectFirstValidOrEmptyText(rateLimitGroup.name(), rateLimitGroup.value());
    }

    private String selectFirstValidOrEmptyText(String ...candidates) {
        for(String candidate : candidates) {
            if(candidate != null && !candidate.isEmpty()) {
                return candidate;
            }
        }
        return "";
    }

    private Node<NodeData<T>> createNodeForGroupOrNull(
            Node<NodeData<T>> parentNode, String name,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimits.length == 0) {
            return null;
        }else{
            return NodeUtil.createGroupNode(parentNode, name, process(rateLimitGroup));
        }
    }

    protected Node<NodeData<T>> createNodeForElementOrNull(
            @Nullable Node<NodeData<T>> parentNode, String name, Object element,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimits.length == 0) {
            return null;
        }else{
            T limit = converter.convert(rateLimitGroup, rateLimits);
            return NodeUtil.createNode(parentNode, name, element, limit);
        }
    }

    private Node<NodeData<T>> requireConsistentData(
            Node<NodeData<T>> rateLimitGroupNode, GenericDeclaration annotatedElement,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimitGroup != null && rateLimits.length != 0) {
            final Operator operator = operator(rateLimitGroup);
            rateLimitGroupNode.getChildren().stream()
                    .map(childNode -> childNode.getValueOptional().orElseThrow(NodeUtil::newExceptionForRequiredValue))
                    .map(NodeData::getValue)
                    .forEach(existing -> requireEqual(annotatedElement, rateLimitGroup, operator, existing));
        }

        return rateLimitGroupNode;
    }

    private Operator operator(RateLimitGroup rateLimitGroup) {
        return rateLimitGroup == null ? AnnotationProcessor.DEFAULT_OPERATOR : rateLimitGroup.logic();
    }

    private void requireEqual(GenericDeclaration annotatedElement, RateLimitGroup rateLimitGroup, Operator lhs, T existing) {
        if(!converter.isOperatorEqual(existing, lhs)) {
            throw new AnnotationProcessingException("Found inconsistent operator, for " +
                    rateLimitGroup + " declared at " + annotatedElement);
        }
    }

    private T process(RateLimitGroup rateLimitGroup) {
        return converter.convert(rateLimitGroup, new RateLimit[0]);
    }
}
