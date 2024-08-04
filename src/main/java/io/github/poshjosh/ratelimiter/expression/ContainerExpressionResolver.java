package io.github.poshjosh.ratelimiter.expression;

import java.util.Collection;
import java.util.Objects;

final class ContainerExpressionResolver extends AbstractExpressionResolver<Object> {
    ContainerExpressionResolver() {}
    @Override
    public boolean resolvePositive(Object left, Operator operator, Object right) {
        if (right instanceof Object[]) {
            for (Object o : (Object[])right) {
                if (Objects.equals(left, o)) {
                    return true;
                }
            }
            return false;
        } else if (right instanceof Collection) {
            return ((Collection)right).contains(left);
        } else if (right instanceof CharSequence) {
            return right.toString().contains(left.toString());
        } else {
            throw Checks.notSupported(this, right);
        }
    }

    @Override
    public boolean isSupported(Operator operator) {
        return operator.equals(Operator.IN);
    }
}
