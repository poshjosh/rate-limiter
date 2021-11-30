# PERFORMANCE STATISTICS

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

### Logs

```
Count: 1000, Spent -> time: 3 millis, memory: 0.0 KB
Count: 1000, Spent -> time: 3 millis, memory: 0.0 KB
Count: 1000, Spent -> time: 3 millis, memory: 0.0 MB
```
```
Count: 10000, Spent -> time: 9 millis, memory: 2.694840 MB
Count: 10000, Spent -> time: 9 millis, memory: 2.694840 MB
Count: 10000, Spent -> time: 9 millis, memory: 2.694840 MB
```
```
Count: 100000, Spent -> time: 25 millis, memory: 5.389680 MB
Count: 100000, Spent -> time: 25 millis, memory: 5.389712 MB
Count: 100000, Spent -> time: 26 millis, memory: 5.389744 MB
```
```
Count: 1000000, Spent -> time: 162 millis, memory: 35.33304 MB
Count: 1000000, Spent -> time: 139 millis, memory: 35.33304 MB
Count: 1000000, Spent -> time: 206 millis, memory: 40.423040 MB
```

## 2 November 2021

Here we compared mutable and immutable implementations of the Rate interface. 

There was little or no differences for the most part. Since it is best practice to use
immutable objects, we stuck with Immutable Rate implementation. However, this library
is designed to allow you easily use your own Rate implementation.

### Immutable Rate instances

__SingletonRateLimiter__

```
Count: 1000, Spent -> time: 57 millis, memory: 2,694 kb
Count: 1000, Spent -> time: 57 millis, memory: 2,694 kb
```
```
Count: 10000, Spent -> time: 55 millis, memory: 2,694 kb
Count: 10000, Spent -> time: 82 millis, memory: 2,694 kb
```
```
Count: 100000, Spent -> time: 64 millis, memory: 8,084 kb
Count: 100000, Spent -> time: 67 millis, memory: 8,084 kb
```
```
Count: 1000000, Spent -> time: 153 millis, memory: 51,202 kb
Count: 1000000, Spent -> time: 171 millis, memory: 51,202 kb
```

__DefaultRateLimiter__

```
Count: 1000000, Spent -> time: 292 millis, memory: 70,554 kb
```

### Mutable Rate instances 

__SingletonRateLimiter__

```
Count: 1000, Spent -> time: 58 millis, memory: 2,695 kb
Count: 1000, Spent -> time: 58 millis, memory: 2,694 kb
```
```
Count: 10000, Spent -> time: 62 millis, memory: 2,694 kb
Count: 10000, Spent -> time: 56 millis, memory: 2,694 kb
```
```
Count: 100000, Spent -> time: 74 millis, memory: 5,389 kb
Count: 100000, Spent -> time: 63 millis, memory: 5,389 kb
```
```
Count: 1000000, Spent -> time: 131 millis, memory: 18,864 kb
Count: 1000000, Spent -> time: 142 millis, memory: 18,864 kb
```

__DefaultRateLimiter__

```
Count: 1000000, Spent -> time: 417 millis, memory: 70,363 kb
```