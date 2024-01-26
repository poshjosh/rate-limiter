package io.github.poshjosh.ratelimiter.expression;

import io.github.poshjosh.ratelimiter.util.StringUtils;

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadInfo;
import java.lang.management.ThreadMXBean;
import java.time.Duration;

final class JvmThreadExpressionParser<S> implements ExpressionParser<S, Object> {

    public static final String COUNT = "jvm.thread.count";
    public static final String COUNT_DAEMON = "jvm.thread.count.daemon";
    public static final String COUNT_DEADLOCKED = "jvm.thread.count.deadlocked";
    public static final String COUNT_DEADLOCKED_MONITOR = "jvm.thread.count.deadlocked.monitor";
    public static final String COUNT_PEAK = "jvm.thread.count.peak";
    public static final String COUNT_STARTED = "jvm.thread.count.started";
    public static final String CURRENT_COUNT_BLOCKED = "jvm.thread.current.count.blocked";
    public static final String CURRENT_COUNT_WAITED = "jvm.thread.current.count.waited";
    public static final String CURRENT_ID = "jvm.thread.current.id";
    public static final String CURRENT_STATE = "jvm.thread.current.state";
    public static final String CURRENT_SUSPENDED = "jvm.thread.current.suspended";
    public static final String CURRENT_TIME_BLOCKED = "jvm.thread.current.time.blocked";
    public static final String CURRENT_TIME_CPU = "jvm.thread.current.time.cpu";
    public static final String CURRENT_TIME_USER = "jvm.thread.current.time.user";
    public static final String CURRENT_TIME_WAITED = "jvm.thread.current.time.waited";

    private static final ThreadMXBean threadMXBean = ManagementFactory.getThreadMXBean();

    JvmThreadExpressionParser() { }

    @Override
    public boolean isSupported(Expression<String> expression) {
        final String lhs = expression.requireLeft();
        switch (lhs) {
            case COUNT:
            case COUNT_DAEMON:
            case COUNT_DEADLOCKED:
            case COUNT_DEADLOCKED_MONITOR:
            case COUNT_PEAK:
            case COUNT_STARTED:
            case CURRENT_COUNT_BLOCKED:
            case CURRENT_COUNT_WAITED:
            case CURRENT_ID:
            case CURRENT_TIME_BLOCKED:
            case CURRENT_TIME_CPU:
            case CURRENT_TIME_USER:
            case CURRENT_TIME_WAITED:
                return expression.getOperator().isType(Operator.Type.COMPARISON);
            case CURRENT_STATE:
            case CURRENT_SUSPENDED:
                return expression.getOperator().equalsIgnoreNegation(Operator.EQUALS);
            default:
                return false;
        }
    }

    @Override
    public Object parseLeft(S context, Expression<String> expression) {
        final String lhs = expression.requireLeft();
        switch (lhs) {
            case COUNT:
                return (long)threadMXBean.getThreadCount();
            case COUNT_DAEMON:
                return (long)threadMXBean.getDaemonThreadCount();
            case COUNT_DEADLOCKED:
                return length(threadMXBean.findDeadlockedThreads());
            case COUNT_DEADLOCKED_MONITOR:
                return length(threadMXBean.findMonitorDeadlockedThreads());
            case COUNT_PEAK:
                return (long)threadMXBean.getPeakThreadCount();
            case COUNT_STARTED:
                return threadMXBean.getTotalStartedThreadCount();
            case CURRENT_COUNT_BLOCKED:
                return threadInfo(context).getBlockedCount();
            case CURRENT_COUNT_WAITED:
                return threadInfo(context).getWaitedCount();
            case CURRENT_ID:
                return getThreadId(context);
            case CURRENT_STATE:
                return threadInfo(context).getThreadState();
            case CURRENT_SUSPENDED:
                return threadInfo(context).isSuspended();
            case CURRENT_TIME_BLOCKED:
                return threadInfo(context).getBlockedTime();
            case CURRENT_TIME_CPU:
                return threadMXBean.getThreadCpuTime(getThreadId(context));
            case CURRENT_TIME_USER:
                return threadMXBean.getThreadUserTime(getThreadId(context));
            case CURRENT_TIME_WAITED:
                return threadInfo(context).getWaitedTime();
            default:
                throw Checks.notSupported(this, lhs);
        }
    }

    private long length(long [] array) {
        return array == null ? 0 : array.length;
    }

    private ThreadInfo threadInfo(S source) {
        return threadMXBean.getThreadInfo(getThreadId(source));
    }

    private long getThreadId(S source) {
        if (source instanceof Long) {
            return (Long)source;
        }
        if (source instanceof String) {
            try {
                return Long.parseLong((String)source);
            } catch(NumberFormatException e) {
                return Thread.currentThread().getId();
            }
        }
        return Thread.currentThread().getId();
    }

    @Override
    public Object parseRight(Expression<String> expression) {
        final String lhs = expression.requireLeft();
        final String rhs = expression.getRightOrDefault("");
        switch (lhs) {
            case COUNT:
            case COUNT_DAEMON:
            case COUNT_DEADLOCKED:
            case COUNT_DEADLOCKED_MONITOR:
            case COUNT_PEAK:
            case COUNT_STARTED:
            case CURRENT_COUNT_BLOCKED:
            case CURRENT_COUNT_WAITED:
            case CURRENT_ID:
                return !StringUtils.hasText(rhs) ? null : Long.parseLong(rhs);
            case CURRENT_STATE:
                return !StringUtils.hasText(rhs) ? null : Thread.State.valueOf(rhs);
            case CURRENT_SUSPENDED:
                return !StringUtils.hasText(rhs) ? null : Boolean.parseBoolean(rhs);
            case CURRENT_TIME_BLOCKED:
            case CURRENT_TIME_CPU:
            case CURRENT_TIME_USER:
            case CURRENT_TIME_WAITED:
                return !StringUtils.hasText(rhs) ? null : Duration.parse(rhs).toMillis();
            default:
                throw Checks.notSupported(this, lhs);
        }
    }
}
