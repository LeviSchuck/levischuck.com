---
title: Asynchronous Systems
---

*When not everything has to happen in
order or at this very moment, you
probably have what could be an
asynchronous system.*

## [Wikipedia Says..][wiki]

> In a synchronous system, operations are
> coordinated under the centralized control
> of a fixed-rate clock signal or several clocks.
> 
> An asynchronous digital system, in contrast, 
> has no global clock: instead, it operates under
> distributed control, with concurrent hardware
> components communicating and synchronizing
> on channels.


## Example

Say we are developing a system that processes
uploads by a user, like [YouTube][]...
Processing a video in the same request as the
upload itself is just not feasible for a lot
of content these days.

What's the solution to that? We break it up,
issue jobs, and tell the user that we have
their content, and to wait a while.
But then, we also want to notify the user
when the video is ready for the public!
That's another signal that gets sent from
one place to another.

When we see that information flows from
a start point, gets analyzed, and causes
a reaction to emit more information, we 
have an asynchronous system.

When we see that we can break a process
up to speed up requests, like the story
behind [beanstalk][], we _can_ have
an asynchronous system.

[beanstalk]: http://kr.github.io/beanstalkd/
[youtube]: http://www.youtube.com
[wiki]: http://en.wikipedia.org/wiki/Asynchronous_system