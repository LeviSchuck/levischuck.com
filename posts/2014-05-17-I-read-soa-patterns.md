---
title: I Read SOA Patterns
---

Since a friend in Utah introduced me to *[SOA Patterns][]* by Arnon Rotem-Gal-Oz, I have been pondering / discovering many of the concepts discussed in this book.
*SOA Patterns* clearly defines many concepts that other books usually only address implicitly. 
Providing clear definitions early on is the best technical writing decision I've seen.

Rotem delineates Service Oriented Architecture ([SOA][]),
providing contexts, solutions, and discussion of how the technology maps the solution to the problem.


## Ponderings

My recent ponderings about designing decoupled systems finally got a name in this book: "choreography".
Rotam lists the benefits and challenges of each pattern, a real time-saver. 

Although I have no intention of writing Simple Object Access Protocol ([SOAP][]) or Web Services Description Language ([WSDL][]) based systems, I have gained a lot of respect for the effort that developers have put into them.
SOA is not an easy problem. 
I suspect that many in the 2010+ scalable software services industry have restricted their choices out of ignorance of the patterns presented in this book.
Perhaps I am not alone in being intimidated by the overhead of SOAP and WSDL. Most developers tend to avoid using tools that involve SOAP and WSDL.

## Recent Movements

Instead, the recent market has gone to "[REST][]ful services" which ironically do not abide by Roy T. Fielding's dissertation.
[Ruby on Rails][ror] and others have unfortunately made REST into a buzzword by diluting its meaning, or by referring to HTTP Create Read Update Destroy ([CRUD][]) APIs as REST.
Through most of the book, Rotem mirrors some of the misunderstandings surrounding REST, but one of the last chapters compares SOA to a wikipedia-copy of [REST][]'s description.

> Hypermedia designs scale better, are more easily changed
> and promote decoupling and encapsulation, with all the
> benefits those things bring.
>
> -- [Steve Klabnik's Hypermedia API Presentation][steve]

It is time we start making [Hypermedia][] [API][]s.
Properly implementing hypermedia products rewards developers with the scalability and ease of integration that true REST promotes. 
Decoupled and evolve-able interfaces let both developers and consumers create robust and flexible applications.
I believe this is where the future of the Web is meant to go--where [mashups][] are everyday things that almost anyone can rapidly make.

But let's not be limited by Request / Reply -centric designs.
We should also consider Event Driven Architecture ([EDA][]) and how we can best align our products with the principles of hypermedia.
If you're interested in learning more, check out the asynchronous SOA patterns discussed by Rotam. 
Namely:

+ Request / Reaction
+ Saga
+ Decoupled Invocation
+ Inversion of Communications

## Event Driven Systems

Let's take a look at [Nobody Needs Reliable Messaging][reliable] by Marc de Graauw.
Marc argues that we should focus on reliability at the business layer, not the transport layer.
Reliance on the transport layer doesn't ensure correct processing and ordering of messages in our business processes.
Idempotence is not merely an abstract virtue to uphold for its own sake--we need it in a business environment to provide the best service.
If Only-Once delivery is impossible to maintain, how can we confirm that the operation's sub-operations all succeeded in an event-decoupled context?
The short answer: the business logic handles that, the transport layer cannot anticipate or know how the application is meant to run.

Most developers who riggorously follow the Don't Repeat Yourself ([DRY][]) principle do their best to remove the need for boilerplate in the first place.
Sometimes developers feel that it is acceptable to hide such implementations in lower layers in the process.
The problem with embedding logic in lower layers is that you give up the choice to have different behavior.
Additionally, such behavior has a limited perspective, since it was forced to only consider a pre-defined sequence of activity.

Along with hypermedia and event driven systems, perhaps we should also consider business-centric primitives to help write robust software.
A topic worthy of its own book.

## Orchestration

SOA Patterns discusses the [Orchestration][] pattern, which explicitly specifies, from an external perspective, how services act and react in a decoupled manner.
Contrast this with "[Choreography][]", in which events are self describing and routing rules can direct events / messages / commands to where they are expected.
Although choreography is a decoupled foundation for event driven systems, choreography is not flexible or configurable by the technical or non-technical teams that decide how data flows.

I believe it may be possible to design orchestration on top of choreography with an additional meta-parameter and a management service.
In my mind, Orchestration resembles the [Actor Model][].
Management actors, whose sole job is to create other actors, often are used to take decoupled computation actors and route the flow between the results.
Each actor-step is unaware of what the next step is.
In [Erlang][], a dominant functional language centered around the actor model, and [Akka][], a JVM actor model library, actors best function as a state machine without using persistant in-memory state.
With each message, the current-state of the state machine may be represented and thus be used by an orchestration engine to determine the next action for long-running computations.

I believe the next steps I will take is to design a choreography engine based on web-hooks, and then lay a foundation or pattern for an orchestration engine on top of it.
I want to stay as close to HTTP as possible, even if the processes may be completely asynchronous.
The goal is to create an interface which can be implimented in nearly every language without significant difficulty, given an open source client library.




[soap]: http://en.wikipedia.org/wiki/SOAP
[wsdl]: http://en.wikipedia.org/wiki/Web_Services_Description_Language
[soa]: http://en.wikipedia.org/wiki/Service-oriented_architecture
[rest]: http://en.wikipedia.org/wiki/Representational_state_transfer
[ror]: http://rubyonrails.org/
[crud]: http://en.wikipedia.org/wiki/Create,_read,_update_and_delete
[api]: http://en.wikipedia.org/wiki/Application_programming_interface
[mashups]: http://en.wikipedia.org/wiki/Mashup_(web_application_hybrid)
[erlang]: http://www.erlang.org/
[akka]: http://akka.io/
[dry]: http://en.wikipedia.org/wiki/Don't_repeat_yourself
[eda]: http://en.wikipedia.org/wiki/Event-driven_architecture
[hypermedia]: http://en.wikipedia.org/wiki/Hypermedia
[orchestration]: http://en.wikipedia.org/wiki/Orchestration_(computing)
[actor model]: http://en.wikipedia.org/wiki/Actor_model
[choreography]: http://en.wikipedia.org/wiki/Service_choreography
[steve]: http://steveklabnik.github.io/hypermedia-presentation/#1
[soa patterns]: http://www.manning.com/rotem/
[reliable]: http://www.infoq.com/articles/no-reliable-messaging