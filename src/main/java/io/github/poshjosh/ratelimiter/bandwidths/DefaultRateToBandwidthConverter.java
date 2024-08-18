package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.model.Rate;
import io.github.poshjosh.ratelimiter.model.Rates;
import io.github.poshjosh.ratelimiter.model.Operator;
import io.github.poshjosh.ratelimiter.util.Ticker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Duration;
import java.util.List;
import java.util.Objects;

final class DefaultRateToBandwidthConverter implements RateToBandwidthConverter {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultRateToBandwidthConverter.class);
    private final Ticker ticker;

    DefaultRateToBandwidthConverter(Ticker ticker) {
        this.ticker = Objects.requireNonNull(ticker);
    }

    @Override
    public Bandwidth convert(Rate rate) {
        if (!rate.isSet()) {
            return Bandwidths.UNLIMITED;
        }
        final BandwidthFactory factory = BandwidthFactories
                .getOrCreateBandwidthFactory(rate.getFactoryClass());
        final long permits = rate.getPermits() < 1 ?
                parsePermits(rate.getRate()) : rate.getPermits();
        final Duration duration = rate.getPermits() < 1 ?
                parseDuration(rate.getRate()) : rate.getDuration();
        return factory.createNew(permits, duration, ticker.elapsedMicros());
    }

    @Override
    public Bandwidth convert(Rates rates, Bandwidth resultIfNone) {
        if (!rates.isSet()) {
            log(rates, resultIfNone);
            return resultIfNone;
        }
        final List<Rate> limits = rates.getRates();
        final Bandwidth[] bandwidths = new Bandwidth[limits.size()];
        for (int i = 0; i < bandwidths.length; i++) {
            Rate rate = limits.get(i);
            bandwidths[i] = convert(rate);
        }
        if (bandwidths.length == 1) {
            log(rates, bandwidths[0]);
            return bandwidths[0];
        }
        final Operator operator =
                Operator.NONE.equals(rates.getOperator()) ? Operator.OR : rates.getOperator();
        final Bandwidth bandwidth = Bandwidths.of(operator, bandwidths);
        log(rates, bandwidth);
        return bandwidth;
    }

    private Duration parseDuration(String rate) {
        final int pivot = rate.indexOf('/');
        return Duration.ofMillis(toDurationMillis(pivot == -1 ? '\u0000' : rate.charAt(pivot + 1)));
    }

    private long parsePermits(String rate) {
        final int pivot = rate.indexOf('/');
        return Long.parseLong(pivot == -1 ? rate : rate.substring(0, pivot));
    }

    private long toDurationMillis(char ch) {
        switch (ch) {
        case '\u0000': return 1;
        case 's': return 1000;
        case 'm': return (60 * 1000);
        case 'h': return (60 * 60 * 1000);
        case 'd': return (24 * 60 * 60 * 1000);
        default: throw new IllegalArgumentException("Invalid duration character: " + ch +
                ", supported characters: '', 's', 'm', 'h', 'd'");
        }
    }

    private static void log(Rates rates, Bandwidth bandwidth) {
        LOG.trace("\n    Rates: {}\nBandwidth: {}", rates, bandwidth);
    }
}
