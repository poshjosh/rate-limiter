package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.Limit;
import com.looseboxes.ratelimiter.node.formatters.NodeFormatters;
import com.looseboxes.ratelimiter.rates.Logic;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.util.Nullable;
import com.looseboxes.ratelimiter.node.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.GenericDeclaration;
import java.util.List;
import java.util.Objects;
import java.util.function.BiConsumer;

public abstract class AbstractAnnotationProcessor<S extends GenericDeclaration> implements AnnotationProcessor<S> {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractAnnotationProcessor.class);

    private final IdProvider<S, String> idProvider;

    protected AbstractAnnotationProcessor(IdProvider<S, String> idProvider) {
        this.idProvider = Objects.requireNonNull(idProvider);
    }

    protected abstract Node<NodeData<Limit>> getOrCreateParent(
            @Nullable Node<NodeData<Limit>> root, S element,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits);

    @Override
    public void process(@Nullable Node<NodeData<Limit>> root, List<S> elements, BiConsumer<Object, Node<NodeData<Limit>>> consumer) {
        elements.forEach(clazz -> process(root, clazz, consumer));
    }

    protected Node<NodeData<Limit>> process(@Nullable Node<NodeData<Limit>> root, S element, BiConsumer<Object, Node<NodeData<Limit>>> consumer){

        final RateLimit [] rateLimits = element.getAnnotationsByType(RateLimit.class);

        final Node<NodeData<Limit>> node;

        if(rateLimits.length > 0 ) {

            RateLimitGroup rateLimitGroup = element.getAnnotation(RateLimitGroup.class);
            Node<NodeData<Limit>> created = getOrCreateParent(root, element, rateLimitGroup, rateLimits);

            Node<NodeData<Limit>> parentNode = created == null ? root : created;
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

    protected Node<NodeData<Limit>> findOrCreateNodeForRateLimitGroupOrNull(
            @Nullable Node<NodeData<Limit>> root, Node<NodeData<Limit>> parent,
            GenericDeclaration annotatedElement, RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        String name = getName(rateLimitGroup);
        final Node<NodeData<Limit>> node;
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

    private Node<NodeData<Limit>> createNodeForGroupOrNull(
            Node<NodeData<Limit>> parentNode, String name,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimits.length == 0) {
            return null;
        }else{
            return NodeUtil.createGroupNode(parentNode, name, createLimit(rateLimitGroup));
        }
    }

    protected Node<NodeData<Limit>> createNodeForElementOrNull(
            @Nullable Node<NodeData<Limit>> parentNode, String name, Object element,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimits.length == 0) {
            return null;
        }else{
            Limit limit = createLimit(rateLimitGroup, rateLimits);
            return NodeUtil.createNode(parentNode, name, element, limit);
        }
    }

    private Node<NodeData<Limit>> requireConsistentData(
            Node<NodeData<Limit>> rateLimitGroupNode, GenericDeclaration annotatedElement,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimitGroup != null && rateLimits.length != 0) {
            final Logic logic = logic(rateLimitGroup);
            rateLimitGroupNode.getChildren().stream()
                    .map(childNode -> childNode.getValueOptional().orElseThrow(NodeUtil::newExceptionForRequiredValue))
                    .map(NodeData::getValue)
                    .forEach(existing -> requireEqual(annotatedElement, rateLimitGroup, logic, existing.getLogic()));
        }

        return rateLimitGroupNode;
    }

    private void requireEqual(GenericDeclaration annotatedElement, RateLimitGroup rateLimitGroup, Logic lhs, Logic rhs) {
        if(!Objects.equals(lhs, rhs)) {
            throw new AnnotationProcessingException("Found inconsistent declaration of logic, for " +
                    rateLimitGroup + " declared at " + annotatedElement);
        }
    }

    private Limit createLimit(RateLimitGroup rateLimitGroup) {
        return createLimit(rateLimitGroup, new RateLimit[0]);
    }

    private Limit createLimit(RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        final Logic logic = logic(rateLimitGroup);
        if (rateLimits.length == 0) {
            return Limit.empty(logic);
        }
        Rate [] rates = new Rate[rateLimits.length];
        for (int i = 0; i < rateLimits.length; i++) {
            rates[i] = createRate(rateLimits[i]);
        }
        return Limit.of(logic(rateLimitGroup), rates);
    }

    private Logic logic(RateLimitGroup rateLimitGroup) {
        return rateLimitGroup == null ? Logic.OR : rateLimitGroup.logic();
    }

    private Rate createRate(RateLimit rateLimit) {
        return Rate.of(rateLimit.limit(), rateLimit.timeUnit().toMillis(rateLimit.duration()));
    }
}
