---
title: Web-hook service specification
---

This post covers my theories for a [stateless][]
event bus as a service strictly on HTTP(S).

The concepts discussed take inspiration from micro
service architectures.
Therefore, authorization and authentication will not
be in the scope of this post, since this service can
be encapsulated within a micro service architecture.


## Language

Let's make sure we start with the same vocabulary.

The key words
"MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL"
in this document are to be interpreted as described in
[RFC2119][] when they appear in ALL CAPS. 
These words may also appear in this document in lower case
as plain English words, absent their normative meanings.

MSA
 ~  Micro service architecture, where a given service will
	do one thing and do that one thing well.
Event
 ~  A payload that is sent or received asynchronously
    often with meta-data which describes *(but not limited to)*

    + Name
    + Sender
    + Time
Sending Context
 ~  The context of a service, which publishes events.
Receiving Context
 ~  The context of a service, which has registered to
    receive events to process.
Cache Service
 ~  Cache services SHOULD provide faster **repeated** access to
    storage-service-backed data.
Storage Services
 ~  
    + MUST persist all data durably
    + SHOULD be fault tolerant and have redundant copies
    + SHALL NOT be concerned with
        - business logic
        - authorization
        - authentication
    + MAY be specialized in what they store.
    + MAY rely on Cache Services when fulfilling requests.
Dead Letters Service
 ~  A dead letters service MAY persist messages and retry sending
    these messages a specified number of times.
    Messages may be permanently marked as dead--never to be sent
    to the original destination service automatically.
    All messages that enter the permanent death state may only be
    analyzed.
    It is NOT RECOMMENDED to manually re-enqueue messages marked
    as permanently dead.
TTL
 ~  Time to live (TTL) or hop limit is a mechanism that limits
    the lifespan or lifetime of data in a computer or network.
    --[Wikipedia](http://en.wikipedia.org/wiki/Time_to_live)


## Design Intent

This service, *herein called the "web-hook service"*

+ SHALL abstract event dispatch from event publishers
+ SHALL abstract event subscription from event processing
+ SHALL NOT provide fanout publish-subscribe functionality
+ SHOULD only be used to communicate between **computational** nodes
+ SHOULD NOT be used to persist the event payload across multiple
    nodes of the same service


## Requirements

### Business Requirements

A reverse proxy service SHOULD be used in coordination with
the web-hook server for authorization and authentication.
Therefore, the web-hook service SHOULD NOT concern itself
with who sends the request.

#### Events

+ MAY specify quality-of-service requirements, such as durability
+ SHOULD NOT be durable by default
+ have OPTIONAL associated (explicitly listed) schemas,
and MAY individually specify meta-information (such as
quality-of-service requirements)
+ schemas MUST have an associated version
+ When the schema version is greater than what cache and storage
    services have available, the event MUST NOT be processed
+ SHOULD have a TTL (Time To Live) specified in the schema;
    the greatest TTL of the schemas will be used to determine
    lifetime of the event

Additionally, quality-of-service settings MAY be specified in the
meta-data on each event, but SHOULD be specified in the schema.

#### Publishers

+ SHALL specify the topic name in the meta data sent with each
    respective event
+ SHOULD specify one-or-more schemas that the event payload
    participates in
+ SHOULD provide applicable versions for each schema that an event
    participates in
+ 

#### Registrant subscribers

+ MUST establish a contract with the web-hook service prior to
    receiving events
    - The contract MUST specify at-least-one filter
    - Each contract MUST specify one-and-only-one destination
    - A filter may be defined as a combination of at-least-one of
        the following where all conditions within each MUST succeed
        in order to be sent to the registrant subscriber
        * A topic name is provided, which MUST be present in all
            messages that are sent to the registrant subscriber
        * A set of referentially transparent invariant assertions
            that apply to the self-describing payload of the message
        * A set of referentially transparent invariant assertions
            that apply to the meta-data of the message
+ MAY specify authentication rules which must be present in all
    subscription fulfillment requests
    - MUST support HTTP Basic authentication
    - SHOULD support appending arbitrary headers
    - SHOULD support appending arbitrary query parameters
    - *Support for [HMAC][] may be discussed at a later time* 
+ MAY specify expected quality-of-service settings, which MUST be
    enacted when contracts are matched to messages

#### The Web-hook service

+ MUST use a storage service for
    - event persistence
    - event schema specification lookup
+ It is RECOMMENDED that distinct typed storage services are used
+ Event schemas MAY be cached
+ If a dead letters service is available, the default mode is that
    messages MAY be sent to the dead-letters service on failure
+ If an event is fully validated, that is, all schemas have been
    loaded correctly, and all quality-of-service specifications have
    been fulfilled, the publisher MUST receive a success response

#### Failure behavior

When the event is refused due to schema-related circumstances as
described above, **one of the following MUST happen**:

+ the event SHALL be sent to a dead-letters service
    and the publisher is to receive a success response
    if the quality-of-service rules specify that a dead letters
    service is acceptable
+ the publisher SHALL receive a failure response and is to act
    accordingly

When a registered subscriber is unable to be contacted, the event
MUST either be dropped, or sent to a dead letters service--depending
on the strongest quality-of-service settings established between
the schema and the registrant's contract.


### Technical Requirements 


Events will be sent to multiple receiving contexts.
However, the contents or context of the event will change
what receiving contexts get notified.



[KRL]: http://en.wikipedia.org/wiki/Kinetic_Rule_Language
[simplepush]: https://wiki.mozilla.org/WebAPI/SimplePush
[gcm]: http://developer.android.com/google/gcm/index.html
[pubsubhubub]: https://code.google.com/p/pubsubhubbub/
[rabbitmq]: https://www.rabbitmq.com/
[apn]: https://developer.apple.com/library/ios/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/ApplePushService.html
[self-describing]: /pages/self-desccribing-data.html
[redis]: http://redis.io/topics/pubsub
[zmq-pub-sub]: http://zguide.zeromq.org/php:chapter5
[apache-esb]: https://synapse.apache.org/
[ibm-esb]: http://www-03.ibm.com/software/products/en/wsesb/
[eca]: http://en.wikipedia.org/wiki/Event_Condition_Action
[ed-soa]: http://en.wikipedia.org/wiki/Event-driven_SOA
[beanstalkd]: http://kr.github.io/beanstalkd/
[message passing]: http://en.wikipedia.org/wiki/Message_passing
[mom]: http://en.wikipedia.org/wiki/Message-oriented_middleware
[esb]: http://en.wikipedia.org/wiki/Enterprise_service_bus
[apache-projects]: https://projects.apache.org/indexes/quick.html
[stateless]: http://en.wikipedia.org/wiki/Stateless_protocol
[rfc2119]: https://www.ietf.org/rfc/rfc2119.txt
[HMAC]: http://en.wikipedia.org/wiki/Hash-based_message_authentication_code
