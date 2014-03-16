---
title: Related Work in Event Driven Architecture
tweets: true
---

Since 2006, large scale systems researchers have
derived or discovered concepts such as
[Event Driven-Service Oriented Architecture][ed-soa]
to solve problems fundamental to creating dependable
distributed systems.
Every solution in this space first defines the problem,
then presents a solution and shows how the problem is solved.
These problems vary from point to point distribution to
agnostic application design.

## Language

Let's make sure we start with the same vocabulary.

<center>
<blockquote class="twitter-tweet" lang="en"><p><a href="https://twitter.com/bfod">@bfod</a> We just prefer to use &quot;device&quot; - it&#39;s all inclusive! :)</p>&mdash; Windows (@Windows) <a href="https://twitter.com/Windows/statuses/436261523997343744">February 19, 2014</a></blockquote>
</center>

Device
 ~  Any user-facing interface, be it the web, a phone,
	a tablet, a desktop is considered a device.

----

## Push Notifications

Push notifications are used to send point to point ephemeral messages
to devices which have already established a contract with the server
for said communication.
For example, if you were just tagged in a picture on Facebook, they
would send your device(s) a push notification stating so.

Although push notifications have been exclusively in mobile contexts,
push notifications have been integrated in nearly all connected
systems.
For example, Mac OS X now comes with Twitter and Facebook
notifications built on Apple's [APN][].

![Mac OS X Notification Center](/images/os-social-integration.png)

Push notifications give services the power to notify human users
of live events.
Notifications may be dropped, and are only delivered at most once.
Push notifications are not used to start application processes.

## Work Queues

Large systems no longer process all the work in a single request.
Unlike simple sites that process a user upload (e.g. an image)
during the request, YouTube defers processing uploaded videos
to a dedicated processing cluster.

With work queues, user facing services can offload and schedule
processing on labeled channels.
Workers will take jobs from these channels, execute the jobs,
and mark the jobs as completed.
If the job fails to be marked as completed within a time interval,
due to the server crashing or otherwise, the job may be re-enqueued
again at a later time.

Work queues are not appropriate for soft real-time contexts.
On their own, work queues only guarantee delivery, watch for
completion, and sometimes re-enqueue if failure is detected.
Work queues do not take into account the subscribed service's loads.

> Its interface is generic, but was originally designed for reducing
> the latency of page views in high-volume web applications by
> running time-consuming tasks asynchronously.
>
> -- [Beanstalk][beanstalkd]

## Publish-Subscribe service

In Publish-Subscribe ("pub-sub") systems, each published message will
be replicated and sent to each subscriber.
Subscriptions are typically partitioned into channels.
A channel typically is a logical identifier that publishers and
subscribers use as an agreed place to deal with messaging.

For example, a channel may exist for `user_1234` we can easily
have the following setup

+ The Instant-Messaging service publishes messages whenever
	`user_1234` gets an instant message to the `user_1234` channel
+ A twitter-mentions service sees that `user_1234` is mentioned
	and aims to let `user_1234` know, so the twitter-mentions service
	publishes a message saying so on the `user_1234` channel
+ `user_1234` is currently on the website
+ `user_1234` has push-notifications setup on his phone
+ `user_1234` also has registered his own personal analysis service
	so he can get a report on what times of day he gets the most
	notifications

With this, we see three types of subscribers, yet they are all
treated the same.
The web page is only around as long as the user has it open.
A service is set up that will forward notifications via push
notifications to his device.
The personal analysis service is likely a web service awaiting data.

In this example, you can see that we have multiple *different* types
of data being published.
It is up to the subscriber in whether to process what it receives or
to ignore it.
Compare this to work queues, where each message should be the same
type; we see that in publish-subscribe systems that this restraint
is (typically) not present.


Services such as [PubNub][] and [Pusher][] provide scalable
pub-sub tools that can be used on a variety of platforms,
such as mobile devices, the web, servers, etc.
We cannot use plain pub-sub when it comes to scheduling work,
as it is considered a waste of resources to do redundant work.

However, this functionality is not exclusive to third party services,
libraries such as [ZMQ][zmq-pub-sub] provide socket interfaces
while databases such as [Redis][] offer this functionality out of
the box.


## Event / Message Middleware

Soft-real time systems use [message passing][] to facilitate
concurrent processes.
Typically, message passing processes run independently
(a [middleware][MOM]) from the product or application.
The benefits of running an independent system for message passing
is scalability, robust delivery, strategic failure handling, and
extensibility.

Solutions such as [RabbitMQ][] provide multiple message exchange
modes.

+ Publish / Subscribe (Fanout)
+ Work Queue (Direct)
+ Filtered Exchange (Topic)

An exchange specifies the run time behavior of how messages are
processed or replicated as they pass between mediating exchanges.
A Publish-Subscribe exchange will send each interested subscriber
node a copy pf the message, where a Work-Queue will send the
message to only one subscribed node.
However, filtered exchanges selectively publish to other exchanges.

Message Middleware enables developers to abstract away the
communication concerns and focus on building resilient systems.


## Enterprise Service Bus

[ESB][]s take messaging middleware to the next level by abstracting
the contractual nature of messages between services.
That is, components can communicate, but not speak the same language.
An Enterprise Service Bus mediates between components, making it
easier to integrate third party services that speak their own
protocols.

> Apache Synapse is a simple and highly effective ESB,
> Web Services intermediary and SOA framework.
> ....
> Once Apache Synapse is mediating your service requests it can
> perform many functions including routing, load-balancing,
> transformation and protocol switching.
> Apache Synapse can be used to build an Enterprise Service Bus
> (ESB) or Service Oriented Architecture (SOA).
>
> -- [Apache Synapse][apache-esb]

Synapse and other ESBs such as [IBM WebSphere][ibm-esb] let developer
teams focus on their own product parts immediately and integration
later.
The most compelling need for an Enterprise Service Bus is integrating
third party components.
For example, when faced with choosing from say 200+
[Apache Top Level Projects][apache-projects],
an ESB gives confidence that integration will not be the deciding
factor of using the technology.

## Conclusion

There are many varieties of event driven system strategies--all with
different principles and use-cases.
Message-Oriented-Middleware and Enterprise-Service-Buses
incorporate the fundamental principles of other styles (i.e.
pub-sub, work queues, etc.), allowing them to suffice for most
large-scale use-cases needing on-the-fly flexibility.
Due to this flexibility and multi-use-case functionality, these
combinatory services often require dedicated infrastructure
to run them adequately.


[ed-soa]: http://en.wikipedia.org/wiki/Event-driven_SOA
[apn]: https://developer.apple.com/library/ios/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/ApplePushService.html
[beanstalkd]: http://kr.github.io/beanstalkd/
[message passing]: http://en.wikipedia.org/wiki/Message_passing
[mom]: http://en.wikipedia.org/wiki/Message-oriented_middleware
[rabbitmq]: https://www.rabbitmq.com/
[esb]: http://en.wikipedia.org/wiki/Enterprise_service_bus
[apache-esb]: https://synapse.apache.org/
[ibm-esb]: http://www-03.ibm.com/software/products/en/wsesb/
[apache-projects]: https://projects.apache.org/indexes/quick.html
[pubnub]: http://www.pubnub.com/
[pusher]: http://pusher.com/
[zmq-pub-sub]: http://zguide.zeromq.org/php:chapter5
[redis]: http://redis.io/topics/pubsub

[simplepush]: https://wiki.mozilla.org/WebAPI/SimplePush
[gcm]: http://developer.android.com/google/gcm/index.html
[pubsubhubub]: https://code.google.com/p/pubsubhubbub/



