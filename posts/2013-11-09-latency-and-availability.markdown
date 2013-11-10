---
title: Latency and Availability
---

While preparing my project page for 
*Solar Wind*, I ended going on a
significant tangent about latency
and availability. I found it best to
cut it out and post it here.

Latencies and availability are often tied
to a [Service Level Agreement][SLA] which
can require a lot of effort to fulfill.

Amazon tries to fulfill their SLA up to
the 99.9th percentile.
For this document, let's say we wish to
be at Amazon's standards.

## Latency
If we want to have shorter latencies, we want
to spend a low amount of time in line to
fulfill our requests. 
We just mentioned a *line*, right?
Well, a simple and obvious principle of some
[queueing theory][mdc] is that if you have
more servers processing requests, then
you'll be able to service more requests
at a time.

Of course, latency has a lower bound on the
processing time itself added to the
transmission time, which may also fluctuate
due to network congestion.

With more queueing theory than I have
at my disposal, you can reason about where
you can best focus your efforts.
Though it would be best to experiment
and get some real data. 


## Availability

What is availability exactly?
According to [Wikipedia][avail]...

> The degree to which a system, subsystem or
> equipment is in a specified operable and
> committable state at the start of a mission,
> when the mission is called for at an unknown,
> i.e. a random, time. Simply put, availability
> is the proportion of time a system is in a
> functioning condition. This is often described
> as a mission capable rate. Mathematically,
> this is expressed as 1 minus unavailability.

How do we achieve high availability though?

If we follow what experienced groups like
[EMC2][] and [Amazon][A2] do, we have

+ Redundancy / Fault Tolerance
+ No single points of failure
+ Recovery Strategies

Amazon advises:

> Assume everything fails and work backwards

What causes downtime?

* Unplanned
    - Human error
    - Hardware failure
    - Software failure
* Planned
    - Upgrades
    - Updates (including your software)

Let's take into account *unplanned* downtime.

### Unplanned Downtime


#### Human Error

Administrators [make mistakes][sorry]. 
The answer? Require less manual
operational involvement for common
but normally risky things.
Basho [does this][basho] with riak.

Simply, automate.

![](/images/automate.jpg)

#### Hardware Failure

It happens all the time.
As Google found out, it is
difficult to [predict][].
The lesson? Expect the
unexpected.

Often, redundancy is the
solution. A prime example
of this is [RAID][]


#### Software Failure

This one is a bit more tricky
since each software is unique.
One technique is to have strategies
to recognize failure, and to
take action, like in the actor
model, to retry or to put it in
a pile later for someone to solve.


It really comes down to planning,
reviewing, lots of testing, and
having it battle tested.


### Planned Downtime

If we use similar strategies
for unplanned downtime, we can be
relatively secure that removing
nodes and replacing nodes which
maintain compatibility will go
without a hiccup.

However, such upgrades must be
done in a planned progression--not
all at once!
Also, they need to be able
to be reverted, especially in
the case of software updates.


[sla]: http://en.wikipedia.org/wiki/Service-level_agreement
[mdc]: http://en.wikipedia.org/wiki/M/D/c_queue
[avail]: http://en.wikipedia.org/wiki/Availability
[emc2]: http://www.slideshare.net/Ciscodatacenter/high-availability-22403142
[a2]: http://www.slideshare.net/AmazonWebServices/architecting-for-high-availability
[sorry]: http://www.slashgear.com/amazon-sorry-for-netflix-downtime-heres-what-we-got-wrong-01262685/
[basho]: http://basho.com/relational-to-riak-part-2-operational-cost-of-scaling/
[predict]: http://www.thewhir.com/blog/googles-study-of-100000-hard-drives-shows-that-disk-failure-is-nearly-impossible-to-predict
[raid]: http://en.wikipedia.org/wiki/RAID

