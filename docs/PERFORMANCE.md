# PERFORMANCE STATISTICS

## 19 January - v0.7.0-SNAPSHOT

#### Invocations of method `RateLimiter#tryAcquire()` with `SmoothBurstyBandwidth`.

| Invocations   | 10k | 100k | 1000k | 
|---------------|-----|------|-------| 
| Time (millis) | 5   | 9    | 42    |
| Memory        | 0   | 0    | 0     |

#### Invocations of method `RateLimiter#tryAcquire()` with `SmoothWarmingUpBandwidth`.

| Invocations   | 10k | 100k | 1000k | 
|---------------|-----|------|-------| 
| Time (millis) | 5   | 7    | 43    |
| Memory        | 0   | 0    | 0     |

#### Invocations of method `RateLimiter#tryAcquire()` with `AllOrNothingBandwidth`.

| Invocations   | 10k | 100k  | 1000k | 
|---------------|-----|-------|-------| 
| Time (millis) | 7   | 17    | 70    |
| Memory        | 0   | 2.7mb | 25mb  |

### Hardware

- Processor Name: __Intel Core i7__
- Processor Speed: __2.6 GHz__
- Number of Processors:	__1__
- Total Number of Cores: __6__
- L2 Cache (per Core):	__256 KB__
- L3 Cache: __12 MB__
- Hyper-Threading Technology: __Enabled__
- Memory: __32 GB__

## 29 December 2022 - v0.0.722-SNAPSHOT

### Summary

Invocations     | 1k | 10k   | 100k  | 1000k  | 
----------------|----|-------|-------|--------| 
Time (millis)   | 6  | 15    | 31    | 102    |
Memory (KB)     | 0  | 0     | 5,000 | 8,000  |

### Hardware

- Processor Name: __Intel Core i7__
- Processor Speed: __2.6 GHz__
- Number of Processors:	__1__
- Total Number of Cores: __6__
- L2 Cache (per Core):	__256 KB__
- L3 Cache: __12 MB__
- Hyper-Threading Technology: __Enabled__
- Memory: __32 GB__


## 29 November 2021 - v0.0.704

### Summary

Invocations     | 1k | 10k   | 100k  | 1000k  | 
----------------|----|-------|-------|--------| 
Time (millis)   | 3  | 9     | 25    | 150    |
Memory (KB)     | 0  | 2,694 | 5,389 | 35,333 |

### Hardware

- Processor Name: __Intel Core i7__
- Processor Speed: __2.6 GHz__
- Number of Processors:	__1__
- Total Number of Cores: __6__
- L2 Cache (per Core):	__256 KB__
- L3 Cache: __12 MB__
- Hyper-Threading Technology: __Enabled__
- Memory: __32 GB__
