---
title: Event Handling
---

As stated previously...

> An event handler receives events asynchronously
> from the source that fired the event.
> 
> It will then process that event and cause a side
> effect.


## Questions from last time

+ How is an event fired?
+ Once an event is fired, how does it get to a handler?
+ How many times would an event be handled?
+ What happens when an event is never handled?
+ What stages might an event pass through before it is handled?


## Preluding thoughts

*Solar Wind* needs to be simple to understand, 
simple to implement, and simple to use.
As I pondered solutions to certain possible features,
the solutions became overly complex, unintuitive,
and started to require assumptions and preconditions
that were too great to require. 

It quickly became obvious that need to be
conservative in the design. Here are some of the
things that will not make it:

+ Optional awaiting event confirmations
    - Pre-receive notification
    - Post-receive notification with status
        (such as success or an exception)
+ Notification when no event has been fired
    within a time range

And some others may easily fall on the list as well.

## Possible models

The first two questions, namely *How is an event fired?*
and *Once an event is fired, how does it get to a handler?*
depend entirely upon the models that would be used.

Let's first start out with conceptually simple models,
where there's little abstraction.

### Direct Call and Recurse

If we treat firing an event as calling a function
with a parameter, then we can also say that when
that handler fires more events, those functions
can be handled too. 

This is not asynchronous, but essentially this is
what we do every day. 
This is also not extensible at all, and requires
that all processing happen on the same machine.
*Although local processing would be desired for
testing purposes, the anticipated environment
will likely include multiple machines with
dedicated directives.*


### Commit to dedicated channels

#### Introduction 

From the [golang book][gobook]

> It would be ideal for programs like these to be
> able to run their smaller components at the same
> time (in the case of the web server to handle
> multiple requests). Making progress on more than
> one task simultaneously is known as concurrency.
> Go has rich support for concurrency using goroutines
> and channels.
>
> ...
> 
> Channels provide a way for two goroutines to
> communicate with one another and synchronize
> their execution.
>
> ...
> 
> A buffered channel is asynchronous; sending or
> receiving a message will not wait unless the
> channel is already full.

If you'd like to see some [pretty pictures][adit],
read the section on actors. 

At this point, I expect you to be aware of
[message passing][wiki], what it means, and
the concepts, since that's essentially what
*Solar Wind* will be facilitating. 

#### Concept implementation

Supposing you have references for any channel
you desire to send events to, firing an
event would be as simple as constructing a
value that represents the event, and submitting
that value to the channel. Once that value is
sent off, the process continues and forgets
that it ever happened.

For now, let's ignore the storage or memory
considerations for a channel.

There is a process that is concurrently listening
to the channel--or is registerred some time
afterwards to drain the channel.
That process gets this event value from the
channel and acts on it.


#### Wait, there's a big gaping hole!

Yeah, about that middle part.. that's where all
sorts of implementations, abstractions, and more
exist. These will be discussed later, as they
are entirely dependent on *how many times an
event is fired*.


## Duplication or no duplication?

Let's first ask,
*How many times would an event be handled?*
Well, that's entirely dependent on the
context, is it not?

So, let's discuss some existing contexts.

*Forewarning: I do not have experience
with the actor model, nor have I used
[rabbitmq][].*

### Actor Model

To my understanding, in the actor model,
an actor can send a message, by value, to
another actor, by using a logical address.
That message is received once and no more.
That actor acts on it, can create actors,
send messages to actors, and change how
the actor will react to the next message.

Effectively, the path that a message goes
is entirely determined beforehand.

### Mail Exchanges

In a mail exchange, like [rabbitmq][],
you have a label that goes on each message,
which while being routed, will go through
an exchange.
This mail exchange has specified rules,
such as direct messaging, fanning out,
and so on. 

Direct messaging is similar to what is
used in actors. A receiver, declares that
it can handle messages of this type. One
receiver amongst the collection will get
the message.

Fanning out is similar to publish-subscribe,
where multiple clients can subscribe and have
a copy of each message as it comes.

Essentially, the exchange which defines the
behavior is decided upon ahead of time. 
Each client that connects to the exchange
*declares* the exchange. If the client
declares an exchange that is not equivalent,
then the client is to raise an error.

This enforces that all clients must agree
on the design, which makes migrations or
upgrades potentially harder.

### Job Queues

A job queue, like [beanstalk][], generally
fulfills the direct messaging aspect, though
it is up to the client or worker to put a
job back if it cannot handle it.

This means that it is up to the developer to
set up multiple beanstalk instances for different
services--to avoid needless job re-scheduling.
The consequence of this means that when ever
you wish for other jobs to happen, you must edit
and redeploy your old code. 

### Conclusions from the above

So far, it seems like everyone anticipates the
knowledge of what the application will do
ahead of when the events get issued.
This is not flexible, fine-grained, or future
proof!

But wait! Let's look here, are these things
dealing with [events][]? At the foundation, I think
each of these methods deliver *things of interest*,
at the core, but with an additional expectation
of how it will be consumed.

Did we just build all that up to fire it down
like a [straw man][]? What we did do was show
what they have in common. 
Each method has a decided contract for how
data will be consumed.

#### Why is pre-runtime agreement important?

Distributed systems that produce and consume
data in incompatible formats or act with
contradictory behavior will likely result
in never ending pain and frustration.

This is why a lot of people focus on control
flow with message systems, actors, and so on.

These models work well in tests, can be easily
reasoned about, and have been battle-tested
by many of the big players.

#### Why is pre-runtime agreement a problem?

When you have to make changes to your
assumptions, alter a design contract, or 
any other significant change that requires
the middle to change, you have to migrate.
Migration will never go away as long as you
are making assumptions about others instead
of yourself.

Sometimes being *too simple* like [beanstalk][]
results in just pushing the overhead to the
user. *The same applies to my experience with
[zeromq][].*

There is power in simplicity! There is choice
in simplicity!

__But where do we draw the line?__

### Back to Duplication

It seems that the consumers should be in
control of what they get. But.. isn't this
what Mail Exchanges have? Yes, but not to
the extent that I am thinking.

Consumers can have conflicts, which is not
what we desire. How can we mediate this?

We know now, that an event should have
a logical identifier to declare what kind
of consumer should consume it.
*This identifier can be thought of as a
topic, though not in the pattern-matching
sense.*

Let's now suppose that each type of
consumer specifies the purpose that it
is listening for
(e.g. "Encode Video X to MP4").
With that information, we can group
together processes that perform
the same function, but listen
for the same data.

## The Plan


Given the following:

+ Our logical destination or *topic* is __T__.
+ When a service instance registers to a topic __T__
    it identifies its *purpose* __P__
    and joins the *exchange* __E__
    on __P__. (i.e. A map of purposes to to
    instances of that service.)
    
+ We have purposed services interested in __T__
    - __A__, which the purpose __a__
    - __B1__, with the purpose __b__
    - __B2__, with the purpose __b__
    - __C1__, with the purpose __c__
    - __C2__, with the purpose __c__
+ Each purpose states the exchange kind
    - __a__ is direct
    - __b__ is direct
    - __c__ is fan-out


Once message __M__ gets to a central
dispatch, we see that __M__ is in
regards to __T__, and the dispatch
is aware that service instances:
__A__; __B1__; __B2__; __C1__; __C2__,
are interested in __T__, then...

1. We receive the message __M__
2. *In the event of any pre-send logic
    like ensuring robustness of messages,
    such would go here.*
3. For each unique purpose __P__
    registered by any number of 
    services __s__ in __E__
4. Pass the event value to the
    exchange type to __E__
    
    When __E__ is
    * __Direct__: then by some algorithm (e.g. LRU)
        we select a service __s__ within
        __E__ and send the event to __s__.

    * __Fan-Out__: then for each service
        __s__ in __E__,
        we send the event to __s__.

    *Other exchanges may be defined at
    a later time, but for now, let's only
    consider the cases that are like a
    job queue and a pub/sub exchange.*

5. *In the event of any post-send logic,
    such as ensuring confirmation of event
    processing... this would go here.*
6. *In the event of further methods that
    partition the topic differently, such would
    go here, which includes steps __4__ and __5__.*
7. Drop message __M__ from memory

Finally, *what happens when an event is
never handled?*
Let's first note that this depends
entirely on the exchange type for each
purpose __P__.
For example, in a direct exchange,
events will pend until they are processed.
In a fan-out exchange, it just gets
dropped.

But what if there are no purposes that
are registered for that topic? Drop it.
Supposing in step __2__, we persist
that we received it in storage, then 
we can replay the events to process
compatible (e.g. direct but not fan-out)
exchanges. 

## Final words

So, on the topic of whether we have
duplication or not, yes we do have
duplication, but the purposes __P__ define
the exchanges that they abide by.

What have we added to an event at this
point? **We have added that an event
needs to have a topic of the kind of
event.** *And this is not like the topic
pattern-matched exchange as detailed
within rabbitmq.*
We have also detailed a small amount on
the *stages an event might pass through
before it is handled*.

### What have we done?
We have partially reversed the roles of
how we process the data. Instead of
sending data which you plan to have processed,
we find it is easier to take inspiration
from [FlightJS][] from twitter, and send
*interesting* data, which can be processed
by any number of services.

We took the concept that [RabbitMQ][] has
for exchanges, added a small layer of
abstraction, and gained behavior that can
still be reasoned about in a large scale,
but be flexible at the fine grained detail.


### What has not been covered?

+ What happens if services of the same purpose
    have different versions?
+ What can we do to ensure robustness of our
    events in the case of failure?
+ How can we try to ensure semantics like
    *only-once*?
+ What will the protocol look like?
+ How can we scale this to multiple servers?

So far, we've sorta talked within the same
process and then crossed some blurry lines
close to being on a remote server.

There's definitely an adventure ahead!







[gobook]: http://www.golang-book.com/10
[adit]: http://adit.io/posts/2013-05-15-Locks,-Actors,-And-STM-In-Pictures.html
[wiki]: http://en.wikipedia.org/wiki/Message_passing
[rabbitmq]: http://www.rabbitmq.com/features.html
[beanstalk]: http://kr.github.io/beanstalkd/
[straw man]: http://en.wikipedia.org/wiki/Straw_man
[events]: /posts/2013-10-30-event-kinds.html
[zeromq]: http://zeromq.org
[flightjs]: http://twitter.github.io/flight/