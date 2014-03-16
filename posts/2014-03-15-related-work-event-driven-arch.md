---
title: Related Work in Event-Driven Architecture
tweets: true
---

Since 2006, large-scale systems designers have
created concepts like
[Event-Driven Service Oriented Architecture][ed-soa]
to solve fundamental problems in constructing dependable
distributed systems.
Every section of this space first defines the problem,
then presents a solution and shows how it is used to solve the problem.
These problems vary from point-to-point distribution to
agnostic application design.

## Language

Let's make sure we start with the same vocabulary.

<center>
<blockquote class="twitter-tweet" lang="en"><p><a href="https://twitter.com/bfod">@bfod</a> We just prefer to use &quot;device&quot; - it&#39;s all inclusive! :)</p>&mdash; Windows (@Windows) <a href="https://twitter.com/Windows/statuses/436261523997343744">February 19, 2014</a></blockquote>
</center>

Device
 ~  Any user-facing interface such as the web, a phone,
	a tablet, or a desktop.

----

##Push Notifications 

Push notifications are used to send point-to-point ephemeral messages
to devices that have already established a contract with the server
of a particular communication source.
For example, if you were just tagged in a picture on Facebook, they
would send your device(s) a push notification to let you know.

Although push notifications have previously been used exclusively in 
mobile contexts, they are now integrated in nearly all connected 
systems.
For example, Mac OS X now comes with Twitter and Facebook
notifications built in on Apple's Push Notification Service [APN][].

![Mac OS X Notification Center](/images/os-social-integration.png)

Push notifications give services the power to notify human users
of live events in real time.
Due to their ephemeral nature, notifications are somewhat unreliable.
They are delivered only once and may be dropped (not delivered).
Push notifications are not used to start application processes.

## Work Queues

Large systems no longer process all the work in a single request.
Unlike simple sites that process a user upload (e.g. an image)
during the request, YouTube defers the processing of uploaded videos
to a dedicated processing cluster.

With work queues, user-facing services can offload and schedule
processing on labeled channels.
Workers will take jobs from these channels, execute the jobs,
and mark the jobs as completed.
If the job fails to be marked as completed within a specified time 
interval due to server issues or other problems, the job may be 
re-enqueued again at a later time.

Work queues are not appropriate for soft real-time contexts.
On their own, work queues only guarantee delivery, monitor
completion, and sometimes re-enqueue if failure is detected.
Work queues do not take into account the subscribed service's loads.

> Its interface is generic, but was originally designed for reducing
> the latency of page views in high-volume web applications by
> running time-consuming tasks asynchronously.
>
> -- [Beanstalk][beanstalkd]

## Publish-Subscribe Service

Publish-Subscribe ("pub-sub") systems replicate published 
messages and send a copy to each subscriber.
Subscriptions are typically partitioned into channels.

A *channel* is a logical identifier that publishers and
subscribers use to sort message delivery. 

For example, a channel created for `user_1234` it may have the following setup:

+ The Instant-Messaging service publishes messages whenever
	`user_1234` receives an instant message on the `user_1234` channel
+ A twitter-mention service sees that `user_1234` is mentioned
	and informs `user_1234` by publishing a message on the `user_1234` channel
+ `user_1234` is currently on the website
+ `user_1234` has push-notifications set up on his phone
+ `user_1234` also has registered his own personal analysis service
	so that he can get a report on what times of day he gets the most
	notifications

As this example shows, `user_1234` may be registered as three types of subscribers. Despite device differences, the messages are all inherently ephemeral.
It is up to the subscriber to decide whether to process what they 
receive or to ignore it.
A web page will only show the current messages as long as the user 
has the web page open. Later views will show newer messages.
Messages sent via push notifications to the user's phone will be 
received once at most.
The personal analysis service is likely a web service awaiting data to collect and process.

Different *types* of data, such as tweets and instant messages, may be published via channels.
Unlike work queues, in which each message should be the same
type, publish-subscribe systems typically do not operate under type constraints.


Services such as [PubNub][] and [Pusher][] provide scalable
pub-sub tools that can be used on a variety of platforms,
such as mobile devices, the web, servers, etc.. 
However, this functionality is not exclusive to third party services.
Libraries such as [ZMQ][zmq-pub-sub] provide socket interfaces,
while databases such as [Redis][] offer this functionality out of
the box.

Plain pub-sub cannot be used when scheduling work queues
since redundant processing is a waste of resources.


## Event / Message Middleware

Soft-real time systems use [message passing][] to facilitate
concurrent processes.
Typically, message passing processes run independently from the product or application via [Message-Oriented middleware][MOM] .
The benefits of running an independent system for message passing
is scalability, robust delivery, strategic failure handling, and
extensibility.

Solutions such as [RabbitMQ][] provide multiple message exchange
modes.

+ Publish / Subscribe (Fanout)
+ Work Queue (Direct)
+ Filtered Exchange (Topic)

An exchange specifies the run-time behavior of messages that are
processed or replicated as they pass between mediating exchanges.
A Publish-Subscribe exchange will send each interested subscriber
node a copy of the message, whereas a Work-Queue will send the
message to only one subscribed node.
However, filtered exchanges selectively publish to other exchanges.

Message Middleware enables developers to abstract away
communication concerns and focus on building resilient systems.


## Enterprise Service Bus (ESB)

[ESB][]s take messaging middleware to the next level by abstracting
the contractual nature of messages between services.
That is, components can communicate without needing to speak the same language.


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

The most compelling use for an Enterprise Service Bus is the integration 
of third party components. An Enterprise Service Bus mediates between components, making it easier to integrate third party services that use different protocols.
For example, when faced with choosing from 200+
[Apache Top Level Projects][apache-projects],
an ESB ensures that integration will not be the deciding
factor for use of a particular technology.

Synapse and other ESBs such as [IBM WebSphere][ibm-esb] let developer
teams immediately focus on their own products and address integration
later.


## Conclusion

There are many varieties of event-driven system strategies--all with
different principles and use-cases.
Message-Oriented-Middleware and Enterprise-Service-Buses
incorporate the fundamental principles of other styles (i.e.
pub-sub, work queues, etc.), allowing them to suffice for most
large-scale use-cases needing on-the-fly flexibility.
Due to their flexibility and multi-use-case functionality, these
combinatory services often require dedicated infrastructure
to run well.


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



