There are two types of events:

1. System-level events
2. Application-level events




## System

System-level events are stored locally, not in git.


### How

- Events are compiled to `system/ledger/*.json`.
- Compiled events are regarded as executed.
- The basename of the file is the **identifier**.

```shell
# Create PostgreSQL database
createdb event-sourcing-experiment

# Compile all events and execute the new ones
./system/assess

# Reset and replay entire history
./system/replay
```




## Application

Application-level events are stored in Redis.


### How

- Each time an event is created it will be pushed to Redis,
  this Redis instance will be used by multiple systems and serves as the ledger.
- A ledger index will be used as the **identifier**.
  This works because the ledger is a list that can only be appended to.
- Redis pub/sub will be used to listen to events.
- Each system that reproduces the state from the ledger stores
  the list index that it last used, so it knows what to execute.

```shell
# Boot up application
./app/boot
```



## Notes

As this is an experiment, there are obviously quite a few things missing. I'm making a list of them here, in case I might use this system in production sometime in the future.

- Database connection pool
- Environment handling (`dev`, `production`, etc)
- Asynchronous error handling


### Asynchronous error handling

Events in the application are handled asynchronously,  
what happens if the processing of an incoming event fails?  
There are few things you could do, depending on your use-case:

- Ignore failures
- Keep a list somewhere of all the failed events, and retry them a few times.
- Stop the entire application.
- ...
