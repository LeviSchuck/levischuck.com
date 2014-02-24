---
layout: post
title: Engine Scripting thoughts
description: My thoughts about separating the engine from the game-specific code
category: Notes
tags: [engine, plans, thoughts, Google Protobuf]
tagline : They say he was at ground zero.
comments: true
---

This might be the dumbest idea ever, but a few developers that I've talked with think it is at least interesting.

## Preface

Earlier, I was considering [AngelScript][] as the scripting language for the games that are used.

Most scripting languages, like [lua][], [squirrel][], and [AngelScript][], and other high level languages like [python][] or [ruby][] depend some form of runtime binding. Which is where all the references to functions and objects are resolved before getting everything started. You can think of it as hooking up an audio system before a big show, there are a lot of cords, and they need to be stable and not have interference issues. 
<!--more-->

## Problem

The problem though is that each way has their own way to hook together. It can be hard between platforms too, or so I hear. To mitigate this, some people use [SWIG][], which creates interfaces to allow a standard means of communication and hooks.

Others use manual bindings that are native to one, like [lua][]. 
Some use third party bindings like [Boost Python][].

Another problem is, that although some of these languages are light, like [lua][], [AngelScript][], and [Squirrel][], debugging, stepping through game code, and so forth usually requires special tools, or custom tools. For example, [lua][] and [AngelScript][] I know for sure provide their own debugging interface.

That's *great* and all if you have the __time and resources__ to be able to make it. 

The last problem is: For the small team that I'm working with, they are not as confident in being able to learn and play with a new language like [python][] or the others. I don't *plan to*, nor *wish to* __code everything__.

## Solution

But, one solution for game code that I'm not aware of being explored is where no bindings are used, and everything is *mostly* asynchronous in nature. 

There will exist the

* Game Engine
* Game Runtime Superior
* Game Runtime Inferior
* Game Interface

They can be different processes, threads, as long as they communicate the same way, the network.

### Wait, what?

Think of it this way, the __Game Interface__ trusts the engine to display stuff, play sounds, load things automatically. It also trusts that the game runtime inferior *or* superior will cause the engine to react to the user input.

The __Game Engine__ trusts that the game interface will process inputs it sends, and that the game inferior *or* superior will tell the engine subsystems when and how to proceed.

#### Each Process

The __Game Engine__ Runs on each instance, it handles all the media, resources, and networking between other engine instances, and the networking with *local* controllers. 

##### Local Controllers 

The __Game Runtime Superior__ is what runs on the host of a game session, it is the master clock for each game session. It simulates / verifies all sessions, and gives messages to the engine to distribute to all other game sessions 

The __Game Runtime Inferior__ is what runs on all non-hosts of a game session, it acts similar to the *Superior* but it only manipulates the engine locally so it feels like there's no lag.
Assuming that the Game Runtimes on both Superior and Inferior have the same data, and the same time, they should behave the same as long as the code is the same. However, the Superior can override the Inferior whenever inconsistencies arise, such as if the client were modified and possibly cheating.

The __Game Runtime__ is what actually executes the rules. Like Unit of type Knight can only move in L patterns on a chess board with a total length of 4.
The Runtime defines how the game acts and continues.

The __Game Interface__ reacts to the input, and handles anything that is presented to the user that is not specific to the entire game for all users. Such as graphics configuration, display menus, etc.

#### All Together

Together, this is what defines a game, a set of intercommunicating processes. 

Why split it up so much?

* Coupling of code can be bad if you want to reuse precious work between projects
* Less limitations later on, because there won't be small assumptions which you have to work around, which cascades later on and repeats. 

But here's the __big part__. Each individual thing can work independently from each other, and be debugged separately from one another. The language does not matter as long as they can talk a similar protocol.

As I've talked earlier, [Google Protobuf][] seems to be a dependable format for standard communications. [Protoc][], the Google Protobuf Compiler, generates for C++, [Java][], and [Python][].

All of which have a robust standard library in varying size.

C++ will be used for the engine, that's not a doubt. Any of the rest could easily as well be written in C++, however it is not a fast language to prototype in. *Memory management and a light standard library as opposed to a fully fleshed out one like what [Java][] or [Python][] has.*

That leaves the *Game Runtime* and *Game Interface* with a choice. 

With regards to a standard library, the **Interface** really should not care, it is completely event driven and states are just fetched from the engine as needed. 

With regards to a standard library, the **Runtime** should care, because the runtime will be processing data, and lots of it, but not resources. It will be processing game data, not subsystem data. An example of subsystem data: physics attributes, like inertia vectors. 

The runtime will be what defines the game by how it acts.
To me, it is important that this is the easiest to prototype, to test, and to iterate.
This desire asks for a managed language, such as [Java][] or [Python][].

Furthermore, both [Java][] and [Python][] have mature debugging environments.
[Eclipse][] and [Java][] are exceptionally integrated, which poses a very positive solution.

*The ability to debug the game code is highly important to me.*

#### Finally

I plan to be mostly working on the engine, which will be in C++. When it comes to the rest, I have more experience with [Java][] than [Python][] because of academic requirements. 
The same is true for other members of the team. 

Therefore, it seems best to not develop for bindings, but asynchronous network calls. 
> Note: You can develop synchronous systems on top of an asynchronous system using acknowledgment messages. 

With the engine being in C++, connecting to a few arbitrary ports on `0.0.0.0`, and components of the game, being the *Interface* and the *Runtime* coded in Java, this should allow for a flexible, debug-able, and in the future, fast-iterating environment. 

#### But why the network?

Because doing Foreign Function Interfaces can be disgusting to deal with, and the network loopback with existing interfaces like [POCO][] and what's built into Java and many other languages gives a dependable interface for communication. 

---

We'll see how this goes.



[AngelScript]: http://www.angelcode.com/angelscript/
[lua]: http://www.lua.org/
[python]: http://www.python.org/
[squirrel]: http://www.squirrel-lang.org/
[ruby]: http://www.ruby-lang.org/en/
[java]: http://en.wikipedia.org/wiki/Java_(programming_language)
[swig]: http://www.swig.org/ "Simplified Wrapper and Interface Generator"
[boost python]: http://www.boost.org/doc/libs/1_51_0/libs/python/doc/
[google protobuf]: https://developers.google.com/protocol-buffers/docs/overview
[protoc]: https://developers.google.com/protocol-buffers/docs/reference/overview
[eclipse]: http://www.eclipse.org/
[poco]: http://pocoproject.org/