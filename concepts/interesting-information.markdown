---
title: Interesting Information
---

*When something happens, announce it.*

*If a service now or later would listen to it,
then it is interesting.*

## Example

Say we are [Facebook][] now. Let's consider
that an arbitrary user *Jessie* adds another
user *James*. Then we expect that James is
notified that Jessie wants to add him.
That information is only interesting to James.

However! When James confirms that request,
they become friends. Now this is interesting
information to many parties. Jessie's friends,
James's friends get a new entry on their feed.
But that's not where the story ends...

Now some services get this interesting
information, "Jessie and James are
friends as of {now}".
Suppose one of these services is a *friendship
recommendation calculator*. It then does
some proprietary magic and comes up with a
list of people to recommend.

At any moment, we can decide to throw
another service that listens to this
*kind of interest*.


[facebook]: https://www.facebook.com
