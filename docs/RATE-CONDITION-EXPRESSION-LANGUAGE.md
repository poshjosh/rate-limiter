# RateCondition Expression Language

A language for expressing the condition for rate limiting.

### Format

An expression is of format `LHS` `OPERATOR` `RHS` e.g `jvm.thread.count.started > 99`

`LHS` = `jvm.thread.count`,  `OPERATOR` = `>`,  `RHS` = `99`

| format                      | example                                              | description                                                         |  
|-----------------------------|------------------------------------------------------|---------------------------------------------------------------------|
| LHS = RHS                   | jvm.thread.count = 22                                | true, when the jvm.thread.count is equal to 22                      |  
| LHS = {key = val}           | sys.environment = {limited = true}                   | true, when system environment named limited equals true             |  
| LHS = [A &#9122; B]         | jvm.current.thread.state = [BLOCKED &#9122; WAITING] | true, when the current thread state is either BLOCKED or WAITING    |
| LHS = {key = [A &#9122; B]} | sys.property = {key_0 = [val_0 &#9122; val_1]}       | true, when either val_0 or val_1 is value of system property key_0  |  
| LHS = {key = [A & B]}       | sys.property = {key_1 = [val_0 & val_1]}             | true, when both val_0 and val_1 are values of system property key_1 |  

### Examples ###

```java
class MatchExamples {
    
    Matcher matchMemoryLessThan1g = Matchers.ofExpression("jvm.memory.available < 1GB");
    
    Matcher matchMemoryLt9gAndTimeElapsedGt2s = Matchers.ofExpression("jvm.memory.available < 9GB & sys.time.elapsed > PT2S");
    
    Matcher matchDeadlockedThreadsGt30 = Matchers.ofExpression("jvm.thread.count.deadlocked > 30");
    
    Matcher matchSysUserNotGuest = Matchers.ofExpression("sys.property = {user.name != guest}");
}
```

### Sample Usage ###

```java
class Resource {

    // 5 permits per second when available system memory is less than 1 GB
    RateLimiter rateLimiter = RateLimiter.of(Bandwidths.ofSeconds(5));
    Matcher matchMemoryLessThanOneG = Matcher.of("jvm.memory.available < 1GB");
    long startTime = System.currentTimeMillis();
    
    String greet(String who) {
        if (matchMemoryLessThanOneG.matches(startTime)) {
            if (!rateLimiter.tryAcquire(1)) {
                throw new RuntimeException("Rate limit exceeded");
            }
        }
        return "Hello " + who;
    }
}
```

### jvm.thread

| name                                   | description |
|----------------------------------------|-------------|
| `jvm.thread.count`                     |             |
| `jvm.thread.count.daemon`              |             |  
| `jvm.thread.count.deadlocked`          |             |
| `jvm.thread.count.deadlocked.monitor`  |             |
| `jvm.thread.count.peak`                |             |
| `jvm.thread.count.started`             |             |
| `jvm.thread.current.count.blocked`     |             |
| `jvm.thread.current.count.waited`      |             |

__jvm.thread(.current).count__ supported format `digits` e.g `128`, `9`

| name                    | description                                   |
|-------------------------|-----------------------------------------------|
| `jvm.thread.current.id` | The id of the thread in digits e.g `128`, `9` |

| name                           | description                                            |
|--------------------------------|--------------------------------------------------------|
| `jvm.thread.current.state`     | `java.lang.Thread.State` of the current thread.        |
| `jvm.thread.current.suspended` | if the current thread is suspended                     |

__jvm.thread.current.state__ supported RHS values [NEW | RUNNABLE |BLOCKED | WAITING | TIMED_WAITING | TERMINATED]

__jvm.thread.current.suspended__ supported RHS values [true | false]

| name                              | description |
|-----------------------------------|-------------|
| `jvm.thread.current.time.blocked` |             |
| `jvm.thread.current.time.cpu`     |             |
| `jvm.thread.current.time.user`    |             |
| `jvm.thread.current.time.waited`  |             |

__jvm.thread.current.time__ supported input formats: ISO-8601 duration format `PnDTnHnMn.nS` with days
considered to be exactly 24 hours. See `java.time.Duration#parse(CharSequence)` for some
examples of this format

### sys.environment

| name              | description                                                         |  
|-------------------|---------------------------------------------------------------------|
| `sys.environment` | Use environment key-value pairs e.g `sys.environment = { LANG = C}` |


### jvm.memory

Support must be provided for the expression. Support is provided by default for the following:

| name                    | description                                                                      |
|-------------------------|----------------------------------------------------------------------------------|
| `jvm.memory.available`  | (_available memory in the JVM i.e. Maximum heap size (`Xmx`) minus used memory_) | 
| `jvm.memory.free`       | (_amount of free memory in the JVM_)                                             |                                     
| `jvm.memory.max`        | (_max amount of memory that the JVM will attempt to use_)                        |                      
| `jvm.memory.total`      | (_total amount of memory in the JVM_)                                            |
| `jvm.memory.used`       | (_total minus free memory_)                                                      |

__jvm.memory__ supported input formats: `digits`, `digits[B|KB|MB|GB|TB|PB|EB|ZB|YB]` 
e.g `1000000`, `1_000_000`, `1GB`, `1gb`

### sys.property

| name           | description                                                            |  
|----------------|------------------------------------------------------------------------|
| `sys.property` | Use property key-value pairs e.g `sys.property = {user.name != guest}` |

### sys.time

| name                | description                              |
|---------------------|------------------------------------------|
| `sys.time`          | (_local date time_)                      |
| `sys.time.elapsed`  | (_time elapsed since application start_) |

__sys.time__ supported input formats: ISO-8601 Time formats:
`uuuu-MM-dd'T'HH:mm`
`uuuu-MM-dd'T'HH:mm:ss`
`uuuu-MM-dd'T'HH:mm:ss.SSS`
`uuuu-MM-dd'T'HH:mm:ss.SSSSSS`
`uuuu-MM-dd'T'HH:mm:ss.SSSSSSSSS`

__sys.time.elapsed__ supported input formats: ISO-8601 duration format `PnDTnHnMn.nS` with days 
considered to be exactly 24 hours. See `java.time.Duration#parse(CharSequence)` for some 
examples of this format.

### Operators

`=`  EQUALS

`>`  GREATER

`>=` GREATER_OR_EQUALS

`<`  LESS

`<=` LESS_OR_EQUALS

`%`  LIKE

`^`  STARTS_WITH

`$`  ENDS_WITH

`!`  NOT (Negates other operators e.g `!=` or `!%`)




 
