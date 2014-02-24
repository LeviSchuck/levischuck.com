---
layout: post
title: LMDVVYD, a Union-Schema contracting database Part 2
date: 2012-11-11 18:42
comments: true
categories: Insane
---

As detailed in the [last post][], I plan to define roughly
how Events work and the Logical layout of the server. 
I plan to defer _How commands are constructed_ and _Return Formats_
for now.

<!--more-->
## Service Layout: Server
I plan for both the server and client to be libraries, and the server
as a stand-alone executable will really be a wrapper that links to it
and broadcasts [ipc][] or [localhost][] or `*`.

The client will have the same interface as the server for the following
kinds of actions

+ Get all entities in a property set
+ Get Raw Entity
+ Create Entity
+ Destroy Entity
+ Create Property
+ Define Property
+ Add Property Set to Entity
+ Remove Property set from Entity, 
	_optional destruction of entity if no properties remain_
+ Some form of serialization for saving or replicating

I want these actions to be present on both the client and the server, because I want the possibility of the client having two paths of communication, one 
for when the server is local, and embedded, and the other for when the server
may be local, or it may be anywhere. This requirement fits well with the
[proxy pattern][].

For the first mode, the client calls the server methods directly. However
when the server is remote, the client uses [ZMQ][] to communicate with the
server, which has a worker that will do the processing. The workers 
following the [thread pool pattern][], otherwise called a worker crew model.

Regarding workers, there is only a finite amount available at a given time,
it may be dynamic, but it needs to be configurable. This is one of the
parameters that should be set when the server serves over the network, 
whether or not it is embedded, since remote clients may want access.
Essentially in an embedded environment though, a client method caller is
their own worker.

## Events

As the actions / methods listed above are executed, events will be emitted,
such as

+ Entities Changed
+ Entity Joined Property Set
+ Entity Left Property Set

There is no need for destruction, since it is not relevant to listeners,
and it can be contained in the _Left Property Set_ event.

The events will go over [ZMQ][], which the **client context** will 
provide when polled for events.

_I'll introduce the client context in a moment_

There would also be some utility functions available, since both
the server and the client should know the contracts, and for safety, I think
there should be a _Conform Entity to Property Set_ function, which should
be used when an entity joins or leaves a property set.

## Service Layout: Client

Within whatever programs that interact with the server, the client 
library offers what the server offers, plus a few things like
subscribing to specific property sets for events, polling these events.

The client library offers what is called a **client context**, 
which has it's own subscribing socket(s), and will automatically filter
to avoid feedback. The context is alive as long as the component
that uses it is alive. Having a context is essentially [RAII][]. It is
up to the programmer to make sure that the components get their event
[messages pumped][message pump], _otherwise stuff will likely stay
and collect in memory._

Ideally, I want the components to rarely depend on the shared
server-client functions, only to be used in special circumstances
like creating new entities, or destroying. The getting should only
be used on initialization. Additionally, the _Get All Entities
in Property Set_ function should have a _serialization debug_ function,
but also one that returns effectively nothing, **but** will cause events
to be emitted, however directed at **only** this context, which will be
handled in the form of _Entity joined property set_, which
will be processed at a later time. 

This makes for reusable [event driven programming][], where initialization
and awareness are handled in one place.

## Multiple Client resolution

I was considering for a while the problem of avoiding conflicts with
different actors, otherwise known as completely unique and separate clients.

One resolution was to have a local database for things, and that we have an 
indirection so that something in the middle resolves what maps from local to
shared, and shared to local. This mapping can be costly since I anticipate
it happening all the time.

Another thought was to have everything on the database's side, and the
database automatically updates their _session_ entities from the shared
entities if the master updates them, but then this loses the entire purpose
of having the communication be local and fast.

Then I thought... Well, if we do follow the event driven pattern, then I can
essentially fake having a local database entirely if I throw one more event
into the design, _Request Entity Knowledge For Property Sets_.

This kind of function would essentially say to every component to emit its
version of the local copy, and then eventually the client context, as the
information comes back in the form of _Entity Joined Property Set_, the
message will have a **[whitelist][]**, containing whoever requested the entity
knowledge. The **client context** will handle the merging once it has
a group of the same entity ID which fit within the property set. All entities
which have part of the entity set, but is a non-empty subset of the 
specified set, which seem to never collect to have the full set will
likely have some sort of expiration date for which the information is discarded.

> Note: The event that an _Entity Joined a Property Set_ has 
> either a **whitelist** for initializations, or a **blacklist**
> to avoid feedback. Also note, that the _[blacklist][]_ must be per
> client instance, not global, since other clients with the same
> component need to be aware of the change! The entry of the
> _blacklist_ or _whitelist_ will be described at the end.


Although this kind of feature is important to having a consistent 
knowledge for when components are dynamically initialized, there
is one huge benefit.

When the master dies, in other words, the game host dies, it is 
**now possible** to have an automated reconstruction for a new 
game host from such fragmented data! And, it might be possible
to even run the game in a crippled mode like this until the master
comes back, or a new master is ready. 

### Blacklists and Whitelists

When it comes to some events, we need a way to feed information
to all but one, or only one. The way we can identify that is through
an ID of sorts. We can't depend on a sequence ID, since this may
cause conflict as components may be initalized nondeterministically.
To resolve this, each context should have it's own [UUID][],
which is a _mostly random_ (except and embedded version number),
128-bit identifier. According to wikipedia..

> Only after generating 1 billion UUIDs every second for the next
> 100 years, the probability of creating just one duplicate would
> be about 50%.

Since There are 
`340,282,366,920,938,463,463,374,607,431,768,211,456` possible UUIDs,
and the chance of collision is low, this is a rather dependable means
to have distributed instances. In fact, this is what many databases,
like [mongoDB][] and [CouchDB][] use to have unique entries created on
multiple servers and still be eventually consistent.

## Initial Prototype

I will probably use [YAML][] as a configuration for a stand alone server,
though I might also try out the things [POCO][] provides for configuration.

The service on initialization will take a parameter specifying which 
_engine_ to use, since there might be many eventually, such as a disk
based one which may be [ACID][]ly robust, or simply efficient for non-critical
data, or even implementations riding on other databases like [mongoDB][]
or even [OrientDB][] which is a graph database.

The first kind will not be super scalable, it will depend on read-write
locks, and use the [STL][] for storage, such as the [red black tree][] map.

It also seems that all classes, such as what defines an entity, property, 
and contracts will need to be implemented for each _data engine_ individually.

_I do not anticipate anything like running multiple engines within the same
server._

----

So, for the next post, I'll describe more about what the _Return Formats_
are for the events, _Commands_ if any, and possibly more details of
an initial implementation.

My Current Todo list now that a lot of other things are depending on
ride on making a template class for representing sets
with the following features: 

+ Add single entry
+ Minus single entry
+ Union with another set
+ Difference of another set
+ Symmetric difference with another set
+ Intersection of another set
+ Powerset of this set

I will likely use a `vector<T>` for storage, and use some of the 
existing standard functions in `<algorithm>`.


[last post]: 2012-11-02-lmdvvyd.html
[ipc]: http://en.wikipedia.org/wiki/Inter-process_communication
[localhost]: http://en.wikipedia.org/wiki/Localhost
[proxy pattern]: http://en.wikipedia.org/wiki/Proxy_pattern
[zmq]: http://www.zeromq.org/
[thread pool pattern]: http://en.wikipedia.org/wiki/Thread_pool_pattern
[raii]: http://en.wikipedia.org/wiki/Resource_Acquisition_Is_Initialization 
[message pump]: http://en.wikipedia.org/wiki/Message_pump
[DML]:http://en.wikipedia.org/wiki/Data_Manipulation_Language
[event driven programming]: http://en.wikipedia.org/wiki/Event-driven_programming
[whitelist]: http://en.wikipedia.org/wiki/Whitelist
[blacklist]: http://en.wikipedia.org/wiki/Blacklist_(computing)
[uuid]: http://en.wikipedia.org/wiki/UUID
[mongodb]: http://www.mongodb.org/
[couchdb]: http://couchdb.apache.org/
[orientdb]: http://www.orientdb.org/index.htm
[yaml]: http://yaml.org/
[poco]: http://pocoproject.org/
[red black tree]: http://en.wikipedia.org/wiki/Red%E2%80%93black_tree
[acid]: http://en.wikipedia.org/wiki/ACID
[stl]: http://en.wikipedia.org/wiki/Standard_Template_Library