---
title: I Just Read SOA Patterns
---

A friend back in Utah introduced me to the book *[SOA Patterns][]* by Arnon Rotem-Gal-Oz.
I have been independently pondering / discovering many of the concepts discussed in this book.
*SOA Patterns* starts the reader off by teaching away from the preconceptions most begin reading with.
What most readers might find to be implicitly intuitive, he gives names for, such as the "Service Host pattern".

Rotem immediately defines what Service Oriented Architecture ([SOA][]) is, and what the goals of SOA are.
Writing immediate definitions is the best decision in technical book writing I've seen.
Rather than lead the reader into the same rut of thought as the author, Rotem presents definitions, contexts, solutions, and how the technology maps the solution to the problem.

## Ponderings

My recent ponderings about designing decoupled systems finally got a name in this book, "choreography".
Rotam listed the benefits and challenges of each pattern, saving me time on my intellectual realizations.

Although I have no intention of writing Simple Object Access Protocol ([SOAP][]) or Web Services Description Language ([WSDL][]) based systems, I have gained a higher respect for the effort that developers have put into them.
SOA is not an easy problem, I feel a lot of the industry has restricted their choices out of ignorance in the 2010+ scalable software services industry.
Perhaps I am not alone in my intimidation of the overhead of SOAP and WSDL--which has lead most developers to ignore the lessons that have been learned.

## Recent Movements

Instead, the recent market has gone to "[REST][]ful services" which ironically do not abide by Roy T. Fielding's dissertation.
[Ruby on Rails][ror] and others have unfortunately made REST into a buzzword by diluting its meaning, or calling HTTP Create Read Update Destroy ([CRUD][]) APIs as REST.
Rotem is not immune to the misunderstandings around REST for the majority of the text, even though one of the last chapters compares SOA to a wikipedia-copy of [REST][]'s description.

> Hypermedia designs scale better, are more easily changed
> and promote decoupling and encapsulation, with all the
> benefits those things bring.
>
> -- [Steve Klabnik's Hypermedia API Presentation][steve]

It is time we start making [Hypermedia][] [API][]s.
Proper implementations intuitively reward the developers with what REST promotes.
Decoupled and evolve-able interfaces let both developers and consumers create robust and flexible applications.
I believe this is where the future of the Web is meant to be--where [mashups][] are an everyday thing that almost anyone can rapidly make.

But let's not be limited by Request / Reply centric designs.
We should also consider Event Driven Architecture ([EDA][]) and how we can best align these principles hypemedia gives us.
For those interested, I advise checking out the asynchronous SOA patterns discussed by Rotam. Namely:

+ Request / Reaction
+ Saga
+ Decoupled Invocation
+ Inversion of Communications

## Event Driven Systems

I think we should also review [Nobody Needs Reliable Messaging][reliable] by Marc de Graauw.
We should focus on reliability at the business layer, not the transport layer.
If we rely only on the transport layer, we cannot adequately ensure correct processing and ordering of messages in our business processes.
Idempotence is not just a virtue we should support, it is something we naturally need in a business environment to provide the best service.
Only-Once delivery is impossible to maintain, how can we confirm if the the operation's sub-operations all succeeded in an evented-decoupled context?
The answer: the business logic handles that, the transport layer cannot anticipate or know how the application is meant to run.

Most that riggorously follow the Don't Repeat Yourself ([DRY][]) principle do their best to remove the need for boilerplate in the first place.
Sometimes developers feel that it is acceptable to hide such implementations in lower layers in the process.
The problem with embedding logic in lower layers is that you give up the choice to have different behavier.
Additionally, such behavior has a limited perspective, since it was forced to only consider a pre-defined sequence of activity.

Along with hypermedia, event driven systems, perhaps we should also consider business-centric primitives to help write robust software.
A topic worthy of its own book.

## Orchestration

While I read SOA Patterns, when I came across the [Orchestration][] pattern, which explicitly specifies, from an external perspective, how services act and react in a decoupled manner.
My ponderings lead me to what was called "[Choreography][]", which is where events are self describing and routing rules can direct events / messages / commands to where they are expected.
Although choreography is a decoupled foundation for event driven systems, choreography is not flexible and configurable for technical or non-technical teams that decide how data flows.

I believe it may be possible to design orchestration on top of choreography with an additional meta-parameter and a management service.
Orchestration has struck a particular resemblance to the [Actor Model][] for me.
Management actors, who's sole job is to create other actors, often are used to take decoupled computation actors and decide the flow between the results.
Each actor-step is unaware of what the next step is.
At least in [Erlang][], a dominant functional language centered around the actor model, as well as [Akka][], a JVM actor model library, actors best function as a state machine without using persistant in-memory state.
With each message, the current-state of the state machine may be represented and thus be used by an orchestration engine to determine the next action for long-running computations.

I believe the next steps I will take is to design a choreography engine based on web-hooks, and upon that lay a foundation or pattern for an orchestration engine.
I want to stay as close to HTTP as possible, even if the processes may be completely asynchronous.
The goal is to create an interface which can be implimented in nearly every language without significant difficulty, given a working client.




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