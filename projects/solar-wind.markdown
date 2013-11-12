---
title: Solar Wind
---

*Solar Wind is a framework to develop
asynchronous systems that revolve around
delivering and reacting to interesting
information.*


## The Problem

We have several existing battle-tested
systems out there that send messages around.
Notably, 

+ [Beanstalk][] A dead-simple job queue.
+ [RabbitMQ][] A distributed robust message queueing service.

However, the current models take the perspective
of designing events for listeners--instead of
emitting generic events for any interested listener
later.

Secondly, these solutions cannot be embedded inside
your application for testing purposes.


## The Solution

Solar Wind will be a library which can be
embedded into any kind of application where
the final use cases for listeners for events
are not entirely known at the beginning.

These applications *for now* will not have
the need or support for confirmations that
certain listeners have received the event.

The mechanism is entirely fire-and-forget.
It is between what a job queue is like beanstalk
and an actor based system like [akka][].




[interesting information]: /concepts/interesting-information.html
[asynchronous systems]: /concepts/asynchronous-systems.html

[beanstalk]: http://kr.github.io/beanstalkd/
[rabbitmq]: http://www.rabbitmq.com/
[akka]: http://doc.akka.io/docs/akka/2.2.3/general/actor-systems.html