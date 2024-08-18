package io.github.poshjosh.ratelimiter.model;

import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * A Rate is a configuration for a rate limit/scope.
 * Rates are used to create Bandwidths. Rates specify the scope/limit of each bandwidth.
 * @see io.github.poshjosh.ratelimiter.bandwidths.Bandwidth
 */
public class Rate implements java.io.Serializable {

    private static final long serialVersionUID = 100L;

    public static final Duration DEFAULT_DURATION = Duration.ofSeconds(1);

    /**
     * Returns a string representation of the rate.
     * 99/m = 99 permits per minute
     * 1    = 1 permit per millisecond
     * Supported characters : '' = milli, 's' = second, 'm' = minute, 'h' = hour, 'd' = day
     *
     * @param permits The number of permits per time unit.
     * @param timeUnit The time unit.
     * @return A string representation of the rate.
     */
    public static String rate(long permits, TimeUnit timeUnit) {
        if (timeUnit == TimeUnit.MILLISECONDS) {
            return "" + permits;
        } else {
            return permits + "/" + toChar(timeUnit);
        }
    }

    public static Rate ofNanos(long permits) {
        return of(permits, Duration.ofNanos(1));
    }

    public static Rate ofMillis(long permits) {
        return of(permits, Duration.ofMillis(1));
    }

    public static Rate ofSeconds(long permits) {
        return of(permits, Duration.ofSeconds(1));
    }

    public static Rate ofMinutes(long permits) {
        return of(permits, Duration.ofMinutes(1));
    }

    public static Rate ofHours(long permits) {
        return of(permits, Duration.ofHours(1));
    }

    public static Rate ofDays(long permits) {
        return of(permits, Duration.ofDays(1));
    }

    public static Rate of(long permitsPerSecond, String condition) {
        return of(permitsPerSecond, DEFAULT_DURATION, condition);
    }

    public static Rate parse(String text) {
        return of(text);
    }

    public static Rate of(String rate) {
        return of(rate, "");
    }

    public static Rate of(long permits, Duration duration) {
        return of(permits, duration, "");
    }

    public static Rate of(String rate, String condition) {
        return of(rate, 0, DEFAULT_DURATION, condition, "");
    }

    public static Rate of(long permits, Duration duration, String condition) {
        return of(permits, duration, condition, "");
    }

    public static Rate of(long permits, Duration duration, String condition, String factoryClass) {
        return of("", permits, duration, condition, factoryClass);
    }

    private static Rate of(
            String rate, long permits, Duration duration, String condition, String factoryClass) {
        return new Rate(rate, permits, duration, condition, factoryClass);
    }

    public static Rate of(Rate rate) {
        return new Rate(rate);
    }

    private String rate;

    private long permits;
    private Duration duration = DEFAULT_DURATION;

    /**
     * An expression which specifies the condition for rate limiting.
     *
     * May be any supported string for example:
     *
     * <p><code>jvm.memory.available < 1_000_000_000</code></p>
     * <p><code>web.request.user.role = ROLE_GUEST</code></p>
     *
     * Support must be provided for the expression. Support is provided by default for the following:
     *
     * <p><code>jvm.thread.count < /code></p>
     * <p><code>jvm.thread.count.daemon < /code></p>
     * <p><code>jvm.thread.count.deadlocked < /code></p>
     * <p><code>jvm.thread.count.deadlocked.monitor < /code></p>
     * <p><code>jvm.thread.count.peak < /code></p>
     * <p><code>jvm.thread.count.started < /code></p>
     * <p><code>jvm.thread.current.count.blocked < /code></p>
     * <p><code>jvm.thread.current.count.waited < /code></p>
     * <p><code>jvm.thread.current.id < /code></p>
     * <p><code>jvm.thread.current.state < /code></p>
     * <p><code>jvm.thread.current.suspended < /code></p>
     * <p><code>jvm.thread.current.time.blocked < /code></p>
     * <p><code>jvm.thread.current.time.cpu < /code></p>
     * <p><code>jvm.thread.current.time.user < /code></p>
     * <p><code>jvm.thread.current.time.waited < /code></p>
     * <p><code>jvm.memory.available < /code></p>
     * <p><code>jvm.memory.free < /code></p>
     * <p><code>jvm.memory.max < /code></p>
     * <p><code>jvm.memory.total < /code></p>
     * <p><code>jvm.memory.used < /code></p>
     * <p><code>sys.time.elapsed < /code></p>
     * <p><code>sys.time < /code></p>
     *
     * Supported operators are:
     *
     * <pre>
     * =  equals
     * >  greater
     * >= greater or equals
     * <  less
     * <= less or equals
     * ^  starts with
     * $  ends with
     * %  contains
     * !  not (e.g !=, !>, !$ etc)
     * </pre>
     * @see Rates#getCondition()
     */
    private String condition;

    /**
     * A class name of a BandwidthFactory that will be dynamically instantiated and used to
     * create Bandwidths from this rate limit. The class must have a zero-argument constructor.
     */
    private String factoryClass;

    public Rate() { }

    Rate(Rate rate) {
        this(rate.rate, rate.permits, rate.duration, rate.condition, rate.factoryClass);
    }

    Rate(String rate, long permits, Duration duration, String condition, String factoryClass) {
        this.rate = rate;
        this.permits = permits;
        this.duration = duration == null ? DEFAULT_DURATION : requirePositive(duration);
        this.condition = condition;
        this.factoryClass = factoryClass;
    }
    private Duration requirePositive(Duration duration) {
        if (duration.isNegative()) {
            throw new IllegalArgumentException("Duration must be positive, duration: " + duration);
        }
        return duration;
    }

    public boolean isSet() {
        return permits > 0 || (rate != null && !rate.isEmpty());
    }

    public Rate rate(String rate) {
        this.setRate(rate);
        return this;
    }

    public String getRate() {
        return rate;
    }

    public void setRate(String rate) {
        this.rate = rate;
    }

    public Rate permits(long permits) {
        this.setPermits(permits);
        return this;
    }

    public long getPermits() {
        return permits;
    }

    public void setPermits(long permits) {
        this.permits = permits;
    }

    public Rate duration(Duration duration) {
        this.setDuration(duration);
        return this;
    }

    public Duration getDuration() {
        return duration;
    }

    public void setDuration(Duration duration) {
        this.duration = duration;
    }

    public Rate condition(String condition) {
        setCondition(condition);
        return this;
    }

    public String getCondition() {
        return condition;
    }

    public void setCondition(String condition) {
        this.condition = condition;
    }

    public Rate factoryClass(String factoryClass) {
        setFactoryClass(factoryClass);
        return this;
    }

    public String getFactoryClass() {
        return factoryClass;
    }

    public void setFactoryClass(String factoryClass) {
        this.factoryClass = factoryClass;
    }

    private static char toChar(TimeUnit ch) {
        switch (ch) {
        case MILLISECONDS: return '\u0000';
        case SECONDS: return 's';
        case MINUTES: return 'm';
        case HOURS: return 'h';
        case DAYS: return 'd';
        default: throw new IllegalArgumentException("Invalid TimeUnit: " + ch +
                ", supported are: MILLISECONDS, SECONDS,  MINUTES, HOURS, DAYS");
        }
    }

    @Override public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;

        Rate rate1 = (Rate) o;

        if (getPermits() != rate1.getPermits())
            return false;
        if (getRate() != null ? !getRate().equals(rate1.getRate()) : rate1.getRate() != null)
            return false;
        if (getDuration() != null ?
                !getDuration().equals(rate1.getDuration()) :
                rate1.getDuration() != null)
            return false;
        if (getCondition() != null ?
                !getCondition().equals(rate1.getCondition()) :
                rate1.getCondition() != null)
            return false;
        return getFactoryClass() != null ?
                getFactoryClass().equals(rate1.getFactoryClass()) :
                rate1.getFactoryClass() == null;
    }

    @Override public int hashCode() {
        int result = getRate() != null ? getRate().hashCode() : 0;
        result = 31 * result + (int) (getPermits() ^ (getPermits() >>> 32));
        result = 31 * result + (getDuration() != null ? getDuration().hashCode() : 0);
        result = 31 * result + (getCondition() != null ? getCondition().hashCode() : 0);
        result = 31 * result + (getFactoryClass() != null ? getFactoryClass().hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "Rate{" +
                ", rate=" + rate +
                "permits=" + permits +
                ", duration=" + duration +
                ", condition=" + condition +
                ", factoryClass=" + (factoryClass == null ? null : factoryClass) +
                '}';
    }
}
