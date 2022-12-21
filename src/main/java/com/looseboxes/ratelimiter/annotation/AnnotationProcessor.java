package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.BandwidthFactory;
import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.node.Node;
import com.looseboxes.ratelimiter.node.formatters.NodeFormatters;
import com.looseboxes.ratelimiter.util.Nullable;
import com.looseboxes.ratelimiter.util.Operator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Objects;
import java.util.function.BiConsumer;

public abstract class AnnotationProcessor<S extends GenericDeclaration> {

    private static final Logger LOG = LoggerFactory.getLogger(AnnotationProcessor.class);

    public static AnnotationProcessor<Class<?>> newInstance() {
        return newInstance(IdProvider.forClass(), IdProvider.forMethod());
    }

    public static AnnotationProcessor<Class<?>> newInstance(
            IdProvider<Class<?>, String> idProviderForClass,
            IdProvider<Method, String> idProviderForMethod) {
        return new ClassAnnotationProcessor(idProviderForClass,
                new MethodAnnotationProcessor(idProviderForMethod));
    }

    private final IdProvider<S, String> idProvider;

    protected AnnotationProcessor(IdProvider<S, String> idProvider) {
        this.idProvider = Objects.requireNonNull(idProvider);
    }

    protected abstract Node<NodeData<Bandwidths>> getOrCreateParent(
            @Nullable Node<NodeData<Bandwidths>> root, S element,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits);

    public void process(Node<NodeData<Bandwidths>> root, List<S> elements) {
        process(root, elements, (element, node) -> {});
    }

    public void process(@Nullable Node<NodeData<Bandwidths>> root, List<S> elements, BiConsumer<Object, Node<NodeData<Bandwidths>>> consumer) {
        elements.forEach(clazz -> process(root, clazz, consumer));
    }

    protected Node<NodeData<Bandwidths>> process(@Nullable Node<NodeData<Bandwidths>> root, S element, BiConsumer<Object, Node<NodeData<Bandwidths>>> consumer){

        final RateLimit [] rateLimits = element.getAnnotationsByType(RateLimit.class);

        final Node<NodeData<Bandwidths>> node;

        if(rateLimits.length > 0 ) {

            RateLimitGroup rateLimitGroup = element.getAnnotation(RateLimitGroup.class);
            Node<NodeData<Bandwidths>> created = getOrCreateParent(root, element, rateLimitGroup, rateLimits);

            Node<NodeData<Bandwidths>> parentNode = created == null ? root : created;
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

    protected Node<NodeData<Bandwidths>> findOrCreateNodeForRateLimitGroupOrNull(
            @Nullable Node<NodeData<Bandwidths>> root, Node<NodeData<Bandwidths>> parent,
            GenericDeclaration annotatedElement, RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        String name = getName(rateLimitGroup);
        final Node<NodeData<Bandwidths>> node;
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

    private Node<NodeData<Bandwidths>> createNodeForGroupOrNull(
            Node<NodeData<Bandwidths>> parentNode, String name,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimits.length == 0) {
            return null;
        }else{
            return NodeUtil.createGroupNode(parentNode, name, createLimit(rateLimitGroup));
        }
    }

    protected Node<NodeData<Bandwidths>> createNodeForElementOrNull(
            @Nullable Node<NodeData<Bandwidths>> parentNode, String name, Object element,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimits.length == 0) {
            return null;
        }else{
            Bandwidths limit = createLimit(rateLimitGroup, rateLimits);
            return NodeUtil.createNode(parentNode, name, element, limit);
        }
    }

    private Node<NodeData<Bandwidths>> requireConsistentData(
            Node<NodeData<Bandwidths>> rateLimitGroupNode, GenericDeclaration annotatedElement,
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

    private Bandwidths createLimit(RateLimitGroup rateLimitGroup) {
        return createLimit(rateLimitGroup, new RateLimit[0]);
    }

    private Bandwidths createLimit(RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        final Operator operator = logic(rateLimitGroup);
        if (rateLimits.length == 0) {
            return Bandwidths.empty(operator);
        }
        Bandwidth[] bandwidths = new Bandwidth[rateLimits.length];
        for (int i = 0; i < rateLimits.length; i++) {
            bandwidths[i] = createBandwidth(rateLimits[i]);
        }
        return Bandwidths.of(logic(rateLimitGroup), bandwidths);
    }

    private Operator logic(RateLimitGroup rateLimitGroup) {
        return rateLimitGroup == null ? Operator.OR : rateLimitGroup.logic();
    }

    private Bandwidth createBandwidth(RateLimit rateLimit) {
        BandwidthFactory bandwidthFactory = BandwidthFactory.getOrCreateBandwidthFactory(rateLimit.factoryClass());
        return bandwidthFactory.createNew(rateLimit.limit(), rateLimit.duration(), rateLimit.timeUnit());
    }
}
