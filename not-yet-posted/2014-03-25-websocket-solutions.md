---
title: Websocket Solutions
---

## Conceptual Example implementations

For example, consider a hierarchical case of a twitter mention.

1. General Case: Statistics notification
2. Relational Case: All followers of the poster see it
3. Specific Case: Mentioned person gets notified by email

Not all tweets actually mention others, so that specific case
would not be considered.

## Dispatch / Broadcast Solutions
Let's now consider a few solutions to this problem.

### Hub-And-Spoke

Every service registers with the web-hook service to be notified
of every event that happens.

Although this can be brute-force-scaled, it sure is not smart.

### Hub-And-Spoke with hinting

Instead of transferring the event contents, we only transfer
meta-data and a storage reference.
Receivers only need to process the meta-data.

This just sends less data, but in the worst case *(where it is
not processed)* depersonalization does not take as high of a toll.

### Static Schema

Every event has an associated set of schemas *(which are unioned)*.
For each schema, an event that explicitly complies with the schema
imply that the event can be processed within an expected context.

Thus, a schema would have a list of interested consumers.

This solution is quite similar to topic exchanges, but with
a contract with type safety.


### Dynamic Rules

Similar to the [Kinetic Rules Engine][KRL], we can use a
declarative syntax to filter and dispatch events.
In other words, an [Event Condition Action][eca] style.


Consider this contrived example.

```sql
-- We want to execute a rule to send a mention
-- notification by calling a specific hook.
select from tweets tweet where tweet.mentions is not null
```

#### Consequences

However, introspection of the event contents (not just context)
requires that the event data (and meta-data) be [self-describing][].

This is an **acceptable requirement** to impose on all users of a
web hook service.


## Events

![](/graphs/event-schema.svg)