package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.RateFactory;
import com.looseboxes.ratelimiter.util.CompositeRate;
import com.looseboxes.ratelimiter.node.formatters.NodeFormatters;
import com.looseboxes.ratelimiter.util.Operator;
import com.looseboxes.ratelimiter.Rate;
import com.looseboxes.ratelimiter.util.Nullable;
import com.looseboxes.ratelimiter.node.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Objects;
import java.util.function.BiConsumer;

public abstract class RateLimitTreeBuilder<S extends GenericDeclaration>
        implements AnnotationTreeBuilder<S> {

    private static final Logger LOG = LoggerFactory.getLogger(RateLimitTreeBuilder.class);

    public static RateLimitTreeBuilder<Class<?>> newInstance(RateFactory rateFactory) {
        return newInstance(IdProvider.forClass(), IdProvider.forMethod(), rateFactory);
    }

    public static RateLimitTreeBuilder<Class<?>> newInstance(
            IdProvider<Class<?>, String> idProviderForClass,
            IdProvider<Method, String> idProviderForMethod,
            RateFactory rateFactory) {
        return new RateLimitTreeBuilderForClass(idProviderForClass, rateFactory,
                new RateLimitTreeBuilderForMethod(idProviderForMethod, rateFactory));
    }

    private final IdProvider<S, String> idProvider;

    private final RateFactory rateFactory;

    protected RateLimitTreeBuilder(IdProvider<S, String> idProvider, RateFactory rateFactory) {
        this.idProvider = Objects.requireNonNull(idProvider);
        this.rateFactory = Objects.requireNonNull(rateFactory);
    }

    protected abstract Node<NodeData<CompositeRate>> getOrCreateParent(
            @Nullable Node<NodeData<CompositeRate>> root, S element,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits);

    @Override
    public void build(@Nullable Node<NodeData<CompositeRate>> root, List<S> elements, BiConsumer<Object, Node<NodeData<CompositeRate>>> consumer) {
        elements.forEach(clazz -> build(root, clazz, consumer));
    }

    protected Node<NodeData<CompositeRate>> build(@Nullable Node<NodeData<CompositeRate>> root, S element, BiConsumer<Object, Node<NodeData<CompositeRate>>> consumer){

        final RateLimit [] rateLimits = element.getAnnotationsByType(RateLimit.class);

        final Node<NodeData<CompositeRate>> node;

        if(rateLimits.length > 0 ) {

            RateLimitGroup rateLimitGroup = element.getAnnotation(RateLimitGroup.class);
            Node<NodeData<CompositeRate>> created = getOrCreateParent(root, element, rateLimitGroup, rateLimits);

            Node<NodeData<CompositeRate>> parentNode = created == null ? root : created;
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

    protected Node<NodeData<CompositeRate>> findOrCreateNodeForRateLimitGroupOrNull(
            @Nullable Node<NodeData<CompositeRate>> root, Node<NodeData<CompositeRate>> parent,
            GenericDeclaration annotatedElement, RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        String name = getName(rateLimitGroup);
        final Node<NodeData<CompositeRate>> node;
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

    private Node<NodeData<CompositeRate>> createNodeForGroupOrNull(
            Node<NodeData<CompositeRate>> parentNode, String name,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimits.length == 0) {
            return null;
        }else{
            return NodeUtil.createGroupNode(parentNode, name, createLimit(rateLimitGroup));
        }
    }

    protected Node<NodeData<CompositeRate>> createNodeForElementOrNull(
            @Nullable Node<NodeData<CompositeRate>> parentNode, String name, Object element,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimits.length == 0) {
            return null;
        }else{
            CompositeRate limit = createLimit(rateLimitGroup, rateLimits);
            return NodeUtil.createNode(parentNode, name, element, limit);
        }
    }

    private Node<NodeData<CompositeRate>> requireConsistentData(
            Node<NodeData<CompositeRate>> rateLimitGroupNode, GenericDeclaration annotatedElement,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimitGroup != null && rateLimits.length != 0) {
            final Operator operator = logic(rateLimitGroup);
            rateLimitGroupNode.getChildren().stream()
                    .map(childNode -> childNode.getValueOptional().orElseThrow(NodeUtil::newExceptionForRequiredValue))
                    .map(NodeData::getValue)
                    .forEach(existing -> requireEqual(annotatedElement, rateLimitGroup, operator, existing.getOperator()));
        }

        return rateLimitGroupNode;
    }

    private void requireEqual(GenericDeclaration annotatedElement, RateLimitGroup rateLimitGroup, Operator lhs, Operator rhs) {
        if(!Objects.equals(lhs, rhs)) {
            throw new AnnotationProcessingException("Found inconsistent declaration of logic, for " +
                    rateLimitGroup + " declared at " + annotatedElement);
        }
    }

    private CompositeRate createLimit(RateLimitGroup rateLimitGroup) {
        return createLimit(rateLimitGroup, new RateLimit[0]);
    }

    private CompositeRate createLimit(RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        final Operator operator = logic(rateLimitGroup);
        if (rateLimits.length == 0) {
            return CompositeRate.empty(operator);
        }
        Rate [] rates = new Rate[rateLimits.length];
        for (int i = 0; i < rateLimits.length; i++) {
            rates[i] = createRate(rateLimits[i]);
        }
        return CompositeRate.of(logic(rateLimitGroup), rates);
    }

    private Operator logic(RateLimitGroup rateLimitGroup) {
        return rateLimitGroup == null ? Operator.OR : rateLimitGroup.logic();
    }

    private Rate createRate(RateLimit rateLimit) {
        return rateFactory.createNew(rateLimit.limit(), rateLimit.timeUnit().toMillis(rateLimit.duration()));
    }
}
