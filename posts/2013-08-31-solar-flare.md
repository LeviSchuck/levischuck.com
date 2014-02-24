---
layout: post
title: Solar Flare
date: 2013-08-31 15:09
comments: true
categories: Solar
---

Over the past few months, I have been learning and trying out
[Haskell][]. I find Haskell to be incredibly terse, comprehensible
when reading, and the confidence given when it compiles is rewarding.

Since about the time of March or so, I have been pondering several
concepts on building a social network that is...

+ Fast
+ Efficient
+ Simple
+ Not limiting
+ Scalable *on pennies*

My conclusion is to build several components that build a capable
foundation for building distributed, simple, and unobtrusive software.

At first, I considered making everything really just one package.
Upon reflection, I realized that this would produce too coupled of
a system and thus later on not fulfill the requirements I desire.

[Pipes][], a library which I investigated recently, really inspires
me to produce clean independent reusable parts. I may not be a
theory heavy guy, but I really appreciate the work that Gabriel
Gonzalez has done. My appreciation for such theory has grown
magnitudes due to my experience with Haskell and libraries like
pipes.

I may not be able to contribute theory-inspired libraries,
but I hope to emulate the style that produces such confidently
correct and beautiful code.

## So what is Solar Flare?

Solar Flare is the name that I am giving to the collection of
components, under the `Solar` namespace, which fulfill the
design goals above.

I already have some code completed, but it does not embody
the goals which I state above. *Not fully anyway.* I will be
reimplementing several parts and release them, as well as
publish to [Hackage][]. 

### What's planned

* Rich Key-Value Schema
	+ Serialize and Deserialize to / from JSON
	+ Relational
	+ Partial Embedded Caches
	+ Simple Conflict Resolution
		- *Possibly automatic [monotonic][] implementations
		  available in later versions*
* Rich Key-Value Storage
	+ Put through all pipes
		- Pipes may have individual configurations,
		  such as [TTL][] for caches
	+ Get
		- First out of any in a pipe
		- Resolved from all storages
	+ Delete
		- Complete Delete / Purge
		- Mark as Deleted / Invalid, but still accessible
	+ Engines
		- In Memory
			+ Persist to disk thread possible
		- Redis
			+ Primarily for caches
		- Riak
* Event Broadcasting Platform
	+ Concepts taken from my [Facebook Theories][] Post
		- Able to be integrated with a request on top of an arbitrary
		  web server in Haskell.
	+ Possible Application for LMDVVYD (See [1][lamed1] [2][lamed2])


More may be added later, but this is the initial todo list.

Plenty of graphs to come later for the event broadcasting platform.

[Haskell]: http://www.haskell.org/haskellwiki/Haskell
[pipes]: http://hackage.haskell.org/package/pipes
[hackage]: http://hackage.haskell.org/packages/hackage.html
[Facebook Theories]: 2013-08-25-facebook-theories.html
[lamed1]: 2012-11-02-lmdvvyd.html
[lamed2]: 2012-11-11-lmdvvyd-part-2.html
[ttl]: http://en.wikipedia.org/wiki/Time_to_live
[monotonic]: http://en.wikipedia.org/wiki/Monotonic_function