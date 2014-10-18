---
title: Solar Orbit
---

*An Amazon Dynamo-style foundation for distributed systems.*

## Inspiration

### Amazon 

Amazon's [DynamoDB][] is a distributed system which is designed
to be highly available and fault tolerant.

> DynamoDB is a fast, fully managed NoSQL database service
> that makes it simple and cost-effective to store and
> retrieve any amount of data, and serve any level of
> request traffic. All data items are stored on
> Solid State Drives (SSDs), and are replicated across 3
> Availability Zones for high availability and durability.

### Basho

[Basho][]'s Riak is inspired by Amazon's [dynamo][] paper, which
details the fundamental concepts behind DynamoDB.

> Riak is a distributed database designed for maximum
> availability: so long as your client can reach one server,
> it should be able to write data. In most failure scenarios
> the data you want to read should be available, albeit
> possibly stale.

The coolest part is how it is masterless.
From [relational to riak][r2r]:

> Riak uses a masterless system with no single point of
> failure, meaning any node can serve read or write requests.
> If a node experiences an outage, other nodes can continue
> to accept read and write requests. Additionally, if a node
> fails or becomes unavailable to the rest of the cluster due
> to a network partition, a neighboring node will take over
> responsibilities for the unavailable node. Once this node
> becomes available again, the neighboring node will pass over
> any updates through a process called “hinted handoff.” This
> is another way that Riak maintains availability and
> resilience even despite serious failure.


## Status Quo

Currently, both Riak and Dynamo are focused on the storage
of things.
Basho [not-so-recently][nsr] released [riak-core][core],
which is intended to be a foundation to write distributed
applications in erlang.

However, there are still leaks in the data structures
for normal [Riak][] present.

DynamoDB is not open source, but at least we have a [paper][dynamo]
that elegantly describes how the foundation works that I am
interested in.

## Goal

Solar Orbit is to be the riak-core for Haskell.
It will be designed with significant influence from the
[sources][core] of riak-core.
A major difference will be that it will not be based
on the erlang actor-model.

[dynamo]: http://www.allthingsdistributed.com/files/amazon-dynamo-sosp2007.pdf
[dynamodb]: http://aws.amazon.com/dynamodb/
[basho]: http://basho.com/
[amazon]: http://www.amazon.com/
[r2r]: http://basho.com/relational-to-riak-part-1-high-availability/
[nsr]: http://basho.com/introducing-riak-core/
[core]: https://github.com/basho/riak_core
[riak]: http://docs.basho.com/riak/latest/