package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.node.formatters.NodeFormatters;
import com.looseboxes.ratelimiter.rates.Logic;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateLimitConfig;
import com.looseboxes.ratelimiter.node.*;

import java.lang.reflect.GenericDeclaration;
import java.util.List;
import java.util.Objects;
import java.util.function.BiConsumer;

public abstract class AnnotationProcessorImpl<S extends GenericDeclaration> implements AnnotationProcessor<S> {

    private static final org.slf4j.Logger LOG = org.slf4j.LoggerFactory.getLogger(AnnotationProcessorImpl.class);

    private final IdProvider<S, String> idProvider;

    public AnnotationProcessorImpl(IdProvider<S, String> idProvider) {
        this.idProvider = Objects.requireNonNull(idProvider);
    }

    protected abstract Node<NodeData> getOrCreateParent(Node<NodeData> root, S element,
                                                           RateLimitGroup rateLimitGroup, RateLimit [] rateLimits);

    @Override
    public Node<NodeData> process(List<S> elements){
        Node<NodeData> rootNode = NodeUtil.getOrCreateRootNode();
        process(rootNode, elements);
        return rootNode;
    }

    @Override
    public void process(Node<NodeData> root, List<S> elements, BiConsumer<Object, Node<NodeData>> consumer) {
        elements.forEach(clazz -> process(root, clazz, consumer));
    }

    protected Node<NodeData> process(Node<NodeData> root, S element, BiConsumer<Object, Node<NodeData>> consumer){

        final RateLimit [] rateLimits = element.getAnnotationsByType(RateLimit.class);

        final Node<NodeData> node;

        if(rateLimits.length > 0 ) {

            RateLimitGroup rateLimitGroup = element.getAnnotation(RateLimitGroup.class);
            Node<NodeData> created = getOrCreateParent(root, element, rateLimitGroup, rateLimits);

            Node<NodeData> parentNode = created == null ? root : created;
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

    protected Node<NodeData> findOrCreateNodeForRateLimitGroupOrNull(
            Node<NodeData> root, Node<NodeData> parent,
            GenericDeclaration annotatedElement, RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        String name = getName(rateLimitGroup);
        final Node<NodeData> node;
        if(rateLimitGroup == null || name.isEmpty()) {
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

    private Node<NodeData> createNodeForGroupOrNull(
            Node<NodeData> parentNode, String name,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimits.length == 0) {
            return null;
        }else{
            return NodeUtil.createGroupNode(parentNode, name, new RateLimitConfig());
        }
    }

    protected Node<NodeData> createNodeForElementOrNull(
            Node<NodeData> parentNode, String name, Object element,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimits.length == 0) {
            return null;
        }else{
            RateLimitConfig rateLimitConfig = toRateLimitConfig(rateLimitGroup, rateLimits);
            return NodeUtil.createNode(parentNode, name, element, rateLimitConfig);
        }
    }

    private Node<NodeData> requireConsistentData(
            Node<NodeData> rateLimitGroupNode, GenericDeclaration annotatedElement,
            RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        if(rateLimitGroup != null && rateLimits.length != 0) {
            RateLimitConfig current = toRateLimitConfig(rateLimitGroup, rateLimits);
            rateLimitGroupNode.getChildren().stream()
                    .map(childNode -> childNode.getValueOptional().orElseThrow(NodeUtil::newExceptionForRequiredValue))
                    .map(NodeData::getConfig)
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

    private RateLimitConfig toRateLimitConfig(RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        RateLimitConfig rateLimitConfig = new RateLimitConfig();
        for (RateLimit rateLimit : rateLimits) {
            RateConfig rateConfig = createRate(rateLimit);
            rateLimitConfig.addLimit(rateConfig);
        }
        return rateLimitConfig.logic(rateLimitGroup == null ? Logic.OR : rateLimitGroup.logic());
    }

    private RateConfig createRate(RateLimit rateLimit) {
        RateConfig rateConfig = new RateConfig();
        rateConfig.setLimit(rateLimit.limit());
        rateConfig.setDuration(rateLimit.duration());
        rateConfig.setTimeUnit(rateLimit.timeUnit());
        return rateConfig;
    }
}
