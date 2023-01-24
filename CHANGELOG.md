# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [ Unreleased ]

-

## [ [0.3.1](https://github.com/poshjosh/rate-limiter/tree/0.3.1) ] - 2023-01-24

### Added

- Implement JVM thread expressions, setup coverage reports

## [ [0.3.0](https://github.com/poshjosh/rate-limiter/tree/v0.3.0) ] - 2023-01-22

### Added

- Add a default operator i.e `Operator.DEFAULT`
- Add more tests
- Improve performance

### Changed

- Rename `Bandwidths.getMembers` to `Bandwidths.getBandwidths`

## [ [0.2.0](https://github.com/poshjosh/rate-limiter/tree/v0.2.0) ] - 2023-01-08

### Changed

- Rename package `com.looseboxes` to `io.github.poshjosh`

### Removed

- Remove methods `RateCache.cache` and `RateCache.listener`

## [ [0.1.0](https://github.com/poshjosh/rate-limiter/tree/v0.1.0) ] - 2023-01-07

### Added

- Add builder like methods to `ResourceLimiter` for setting `RateCache` and `UsageListener`

### Changed

- Update README.md
- Rename all `of()` to `ofDefaults()`

### Removed

- Remove annotation `@Nullable`
- Remove the value-related generic from `RateCache`. The value now has a fixed type of `Bandwidths`

## [ [0.0.9](https://github.com/poshjosh/rate-limiter/tree/v0.0.9) ] - 2022-12-30

### Added

- Add more Bandwidth tests

## [ [0.0.8](https://github.com/poshjosh/rate-limiter/tree/v0.0.8) ] - 2022-12-29

### Added

- Separate repository for annotations: [rate-limiter-annotation](https://github.com/poshjosh/rate-limiter-annotation)
- Change log
