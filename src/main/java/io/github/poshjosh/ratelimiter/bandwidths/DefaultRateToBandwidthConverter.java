package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.model.Rate;
import io.github.poshjosh.ratelimiter.model.Rates;
import io.github.poshjosh.ratelimiter.util.Operator;
import io.github.poshjosh.ratelimiter.util.Ticker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
        final BandwidthFactory factory = BandwidthFactories
                .getOrCreateBandwidthFactory(rate.getFactoryClass());
        return factory.createNew(rate.getPermits(), rate.getDuration(), ticker.elapsedMicros());
    }

    @Override
    public Bandwidth convert(Rates rates, Bandwidth resultIfNone) {
        if (!rates.hasLimits()) {
            log(rates, resultIfNone);
            return resultIfNone;
        }
        final List<Rate> limits = rates.getAllLimits();
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
        final String id = BandwidthArray.buildId(operator, bandwidths);
        final Bandwidth bandwidth = Bandwidths.of(id, operator, bandwidths);
        log(rates, bandwidth);
        return bandwidth;
    }

    private static void log(Rates rates, Bandwidth bandwidth) {
        LOG.trace("\n    Rates: {}\nBandwidth: {}", rates, bandwidth);
    }
}
