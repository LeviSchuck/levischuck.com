---
layout: post
title: Getting Started
description: ""
category: General
tags: [engine, entity system, definitions, introduction, plans]
tagline: Welcome, welcome to city 17.
comments: true
---

## Who am I?
I am Levi, a somewhat self-taught developer.

I am fluent and have experience with the following languages

* VB.NET, professional, some personal
* C++, academic, personal
* PHP, professional, some personal

I have some experience with the following languages

* Opa, personal
* Java, academic only
* Python, Tinkered around
* Ruby, Tinkered around

I do web development and programming as well, specifically professionally, so I have experience with

* ASP.NET
* CSS
* jQuery
* PHP
* javascript
<!--more-->

## What do I want to do?
I want to make a game-agnostic engine framework. 

### What does this mean?
It means that games, or even interactive films, are not defined by the limitations of the engine. The engine exists to facilitate the things which about everything needs, which is the ability to save, load, communicate, manage memory, and handle resources.

### So, if the engine is just a foundation, how are games made?
Games will be made by adding [subsystems][subsystem] to the engine as needed.
Subsystems are things like networking, 2D graphics, etc. 
Furthermore, games will follow the concept of an entity system. All things that can be considered individual are [entities][entity]. Most game programming for what defines the game will be in the [components][component], for example, defining how a player might walk around, or interfacing with a [subsystem][].

[entity]: http://en.wikipedia.org/wiki/Entity_(disambiguation) "Entity in concept"
[component]: http://en.wikipedia.org/wiki/Component-based_software_engineering "Component Based Software Engineering"
[subsystem]: http://en.wikipedia.org/wiki/Subsystem "Subsystem definition"

### What is a subsystem?
A subsystem is a feature with an interface that provides and exposes functionality to an engine.

A subsystem might be a 3D physics engine, or a 2D graphics interface.

### What are entities?
A single unit of something. In the engine it is simply defined by an arbitrary number.

However, [entities][entity] have a table of attributes. Attributes are arbitrary variables which are what will be saved when [serialized][serialize]. Any [component][] may modify the attributes that are owned by the entity, however altering the contents is not synchronous between all systems, because some may still be reading the attributes on another processor.

To facilitate the safety of memory, a [message system][message passing] will be used, we'll also be using the same means for communication between servers and clients using a [dispatcher][dynamic dispatch].

[Entities][entity] will have a properties list, which define what [components][component] are registered, by adding a property, the [component system][component] becomes aware and will process the entity every time it is called to process all of its members. Properties can be added and removed at runtime.  

[serialize]: http://en.wikipedia.org/wiki/Serialize "Serialize definition"
[message passing]: http://en.wikipedia.org/wiki/Message_passing "Message System concept"
[dynamic dispatch]: http://en.wikipedia.org/wiki/Dynamic_dispatch "Dynamic Dispatcher concept"

### What are components?
[Components][component] are units of functioning logic which operate on its own members. [Components][component] usually do not take into account the properties listed in the [entity][]. Thus most should be considered unaware of the other components. This is desired because it means we will be able to spot bugs much easier and be able to resolve them speedily because the logic is not tangled with other logic which should not be concerned with each other.

[Components][component] may be defined by the game code or engine. 

An engine [component][] would be something that requires efficiency and possibly a lot of data processing, rather than game specific logic. For example, a bullet physics rigid body component.

A game [component][] would be something that is mainly specific to the game, like defining if a unit can fly, or if a unit goes on a special tile it gets a bonus.

One thing to note is that components will have their own [serialization][serialize] step as well. Some data would be too inefficient to access and modify if stored in an attributes table, and are only important internally. Such components might have an interface to allow other components to read when necessary. 
The case I have for this is a 2D map for what tiles are defined for which sectors.

### What is a message system?
A [Message System][message passing] is how components might be able to talk to each other, without concerning themselves of the source. For example, a 3D graphics [subsystem][] should not concern itself with what set the position, be it scripting, or physics, or player interaction. 

A [message][message passing] consists of a set of types or categories, the associated data, a priority, and a time stamp.

[Messages][message passing] will be used to modify the Attributes. There will be a read / write lock on the attributes, so writing will be asynchronous until other components are done reading for the moment. Additionally, based on the types defined, another component might be subscribed to such an update, for example *position transformation*, or *unit death*.

This allows for a [reactive][reactive system] [system][reactive programming] design of individual events, instead of checking for a change of something through everything. 

[reactive system]: http://en.wikipedia.org/wiki/Reactive_system "Reactive System system engineering definition"
[reactive programming]: http://en.wikipedia.org/wiki/Reactive_programming 

## How am I going to do it?
The engine core will be programmed in C++, which will be greatly accelerated with [POCO][].

Messages will be defined using [Google Protobuf][protobuf].

Data for definitions of a game will be using [YAML][].

Scripting for a game will be using [AngelScript][angelcode].

Some graphics and audio will be provided with [SFML][sfml].

[poco]: http://pocoproject.org/ "POCO Libraries"
[protobuf]: https://developers.google.com/protocol-buffers/docs/overview "Google Protobuf Serialization"
[yaml]: http://www.yaml.org/ "YAML Ain't a Markup Language"
[angelcode]: http://www.angelcode.com/
[sfml]: http://www.sfml-dev.org/ "Simple and Fast Multimedia Library"