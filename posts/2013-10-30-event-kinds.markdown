---
title: Event Kinds
---

As of late, I have been pondering the foundation
for an event framework, which I plan to call
*Solar Wind*.

What **is** an event?
What is **in** an event?
What **causes** an event to happen?
What **reacts** to an event?

Many more questions have filled my mind, as my goal
is to not just make a clone of something like [RabbitMQ][].
I need to understand the full context before I
start making assumptions or conclusions as to
what will make a good design.

<!-- more -->

## Questions

+ What is an event?
+ What is in an event?
+ What causes an event to happen?
+ What reacts to an event?
+ Where might an event go?
+ How long does an event need to last?
+ How will the event be consumed?

## Event Definition

An event is a notification that something 
interesting happened.
An event may be caused by a user interaction,
a regularly occurring process, or be the
product of a previous event being processed.

The term, *firing an event* means that the
event is created and sent to be dispatched.

What is an event dispatcher?
A logically centralized broker that receives
events and forwards these events to interested
listeners.
Events that are forwarded are received by an
*event handler*.
An event handler receives events asynchronously
from the source that fired the event.
It will then process that event and cause a side
effect.

## Event Contents

An event handler, as a consumer of an event,
in most cases expects a context of the event.
The context encapsulates what makes the event
interesting.
However, the context is inherently going to
differ between event types.

Although the context is enough to act, some
meta-data would be beneficial for logging
and debugging.
In dissecting an event in our effectual world,
we have that:

+ Events are fired at some time in the world.
+ Events may be caused by other events.
+ Events have a root cause action.

*While pondering whether or not an event can have
multiple causing events, it occurred to me that
such would involve state.
I find that state should and will not be in the
scope of my project.
Especially in asynchronous systems where some
states may never become evaluated to completion.*

### Meta information

Therefore, let's impose an additional constraint.
The cause of an event may be of the following:

+ A previous event
+ An external event
    - A user action
    - Reoccurring / Scheduled process
    - Machine event

A *machine event* pertains to something that is
not within the scope of the applications involved,
such as infrastructure events.

Now let's examine what these causes have in common.
Each event, regardless of cause, happens at a
time and place. A place being a logical machine
and process.

Thus, as meta information, we add the following
contents:

+ Time
+ Machine
+ Process

Further, we have a constraint that has not been
discussed, namely: identifying the cause.

If a previous event lead to this event we need
something to uniquely identify that event. 

Therefore, let's add a unique identifier to the
contents, besides the payload, we now have:

+ Time
+ Machine
+ Process
+ Unique Identifier

### Cause 
Now to satisfy the constraint, we must have
one piece that describes the cause.
A discriminated unions seems best for this.
One union that would satisfy this constraint is:

+ Previous Event: Event Identifier
+ Machine: Event Classification, depends upon
    the implementation
+ Infrastructure Event: depends upon the implementation
+ Scheduled: Scheduling descriptor of some kind,
    depends on the final implementation
+ Application: Depends on the implementation

### Current Contents

Thus we have that an event contains: 

+ Payload
+ Time
+ Machine
+ Process
+ Unique Identifier
+ Cause

## Event Handling
Pertaining to the question of what reacts to an event,
I previously noted that:
An event handler receives events asynchronously
from the source that fired the event.
It will then process that event and cause a side
effect. An event will be consumed by a handler.

### Side Effects
A side effect may a sequence of any of the following:

+ A transactional database operation.
+ Pushing content to a foreign entity, such as a user.
+ A request to a foreign system.
+ Modifying, creating, or deleting content on a file system.
+ Firing another event.
+ Doing nothing, which is the identity of a side effect.

### Handler
The structure of a handler is dependent upon the method of
handler routing. So I will defer this.

## Final Notes
This document has taken four days, on and off, to write.
It seems best that I separate the concepts of the
abstract event and how such events are handled.

### Questions remaining

+ How is an event fired?
+ Once an event is fired, how does it get to a handler?
+ How many times would an event be handled?
+ What happens when an event is never handled?
+ What stages might an event pass through before it is handled?


[rabbitmq]: http://www.rabbitmq.com/
[pure]: http://en.wikipedia.org/wiki/Pure_function