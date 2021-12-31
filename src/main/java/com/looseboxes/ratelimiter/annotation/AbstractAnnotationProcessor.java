package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.formatters.NodeFormatters;
import com.looseboxes.ratelimiter.rates.Logic;
import com.looseboxes.ratelimiter.util.Nullable;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateConfigList;
import com.looseboxes.ratelimiter.node.*;

import java.lang.reflect.GenericDeclaration;
import java.util.List;
import java.util.Objects;
import java.util.function.BiConsumer;

public abstract class AbstractAnnotationProcessor<S extends GenericDeclaration> implements AnnotationProcessor<S> {

    private static final org.slf4j.Logger LOG = org.slf4j.LoggerFactory.getLogger(AbstractAnnotationProcessor.class);

    private final IdProvider<S, String> idProvider;

    public AbstractAnnotationProcessor(IdProvider<S, String> idProvider) {
        this.idProvider = Objects.requireNonNull(idProvider);
    }

    protected abstract Node<NodeValue<RateConfigList>> getOrCreateParent(
            @Nullable Node<NodeValue<RateConfigList>> root, S element,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits);

    @Override
    public void process(@Nullable Node<NodeValue<RateConfigList>> root, List<S> elements, BiConsumer<Object, Node<NodeValue<RateConfigList>>> consumer) {
        elements.forEach(clazz -> process(root, clazz, consumer));
    }

    protected Node<NodeValue<RateConfigList>> process(@Nullable Node<NodeValue<RateConfigList>> root, S element, BiConsumer<Object, Node<NodeValue<RateConfigList>>> consumer){

        final RateLimit [] rateLimits = element.getAnnotationsByType(RateLimit.class);

        final Node<NodeValue<RateConfigList>> node;

        if(rateLimits.length > 0 ) {

            RateLimitGroup rateLimitGroup = element.getAnnotation(RateLimitGroup.class);
            Node<NodeValue<RateConfigList>> created = getOrCreateParent(root, element, rateLimitGroup, rateLimits);

            Node<NodeValue<RateConfigList>> parentNode = created == null ? root : created;
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

    protected Node<NodeValue<RateConfigList>> findOrCreateNodeForRateLimitGroupOrNull(
            @Nullable Node<NodeValue<RateConfigList>> root, Node<NodeValue<RateConfigList>> parent,
            GenericDeclaration annotatedElement, RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        String name = getName(rateLimitGroup);
        final Node<NodeValue<RateConfigList>> node;
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

    private Node<NodeValue<RateConfigList>> createNodeForGroupOrNull(
            Node<NodeValue<RateConfigList>> parentNode, String name,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimits.length == 0) {
            return null;
        }else{
            return NodeUtil.createGroupNode(parentNode, name, new RateConfigList());
        }
    }

    protected Node<NodeValue<RateConfigList>> createNodeForElementOrNull(
            @Nullable Node<NodeValue<RateConfigList>> parentNode, String name, Object element,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimits.length == 0) {
            return null;
        }else{
            RateConfigList rateConfigList = toRateLimitConfig(rateLimitGroup, rateLimits);
            return NodeUtil.createNode(parentNode, name, element, rateConfigList);
        }
    }

    private Node<NodeValue<RateConfigList>> requireConsistentData(
            Node<NodeValue<RateConfigList>> rateLimitGroupNode, GenericDeclaration annotatedElement,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimitGroup != null && rateLimits.length != 0) {
            RateConfigList current = toRateLimitConfig(rateLimitGroup, rateLimits);
            rateLimitGroupNode.getChildren().stream()
                    .map(childNode -> childNode.getValueOptional().orElseThrow(NodeUtil::newExceptionForRequiredValue))
                    .map(NodeValue::getValue)
                    .forEach(existing -> requireEqual(annotatedElement, rateLimitGroup, current.getLogic(), existing.getLogic()));
        }

        return rateLimitGroupNode;
    }

    private void requireEqual(GenericDeclaration annotatedElement, RateLimitGroup rateLimitGroup, Logic lhs, Logic rhs) {
        if(!Objects.equals(lhs, rhs)) {
            throw new AnnotationProcessingException("Found inconsistent declaration of logic, for " +
                    rateLimitGroup + " declared at " + annotatedElement);
        }
    }

    private RateConfigList toRateLimitConfig(RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        RateConfigList rateConfigList = new RateConfigList();
        for (RateLimit rateLimit : rateLimits) {
            RateConfig rateConfig = createRate(rateLimit);
            rateConfigList.addLimit(rateConfig);
        }
        return rateConfigList.logic(rateLimitGroup == null ? Logic.OR : rateLimitGroup.logic());
    }

    private RateConfig createRate(RateLimit rateLimit) {
        RateConfig rateConfig = new RateConfig();
        rateConfig.setLimit(rateLimit.limit());
        rateConfig.setDuration(rateLimit.duration());
        rateConfig.setTimeUnit(rateLimit.timeUnit());
        return rateConfig;
    }
}
