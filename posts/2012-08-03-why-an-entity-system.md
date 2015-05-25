---
layout: post
title: Why an Entity System?
description: What is an entity system, why do I want it, why is it useful?
category: General
tags: [engine, entity system]
tagline: Please disregard any undeserved complements.
comments: true
---


My last post explained in summary what an [entity system][] is, 
however a few wondered the questions along the lines of

* Why do I want to do it?
* How is it useful?

And maybe the questions

* I'm not a programmer, but I lead a development team, why would this be good for me?
* How will this speed up my game development?
* How much will this system cost us in time?

[entity system]: http://en.wikipedia.org/wiki/Entity-relationship_model
<!--more-->

## Main Questions

### Why do I want to do it?
Why do I, the programmer, want to make an [entity system][] rather than go for an [object oriented design][] with [class diagrams][]?

Let me start by showing an example of what commonly goes wrong.

In a game, one might have a simple game object, many things extend this game object, maybe vehicle, person, or just an autonomous being.
Let's focus on the autonomous being, from that, could be extended a human NPC, an animal, an enemy and so on. 
The problem becomes that when you have a human, and an enemy, and you want both to have certain features, you'll have to put the code in two places, which might get messy individually and no longer can be merged or refactored. So, you have the choice of duplicating the code and maintaining two copies of something, or you could push the feature code to the parent, being the autonomous being. Then the animal magically gets this functionality. Do you then override it so it does nothing? 

You end up getting this list of things where you have "I want these features, but the *plans* I chose come up with these other *features* that I don't want present!", so you end up with overhead of adding and hiding features that have been inherited.

Or, a feature gets left in unintended which may only manifest itself in a very specific case not thought of to test for play testing. My use case here is than in [Skyrim], a horse and a chicken can observe a player stealing, and then the police are automatically notified and you can get arrested. This lack of foresight has led to many comical depictions.

![Detected by a horse](/images/detected-by-horse.png)

Also, consider that if properties to an entity were just a flat collection of things, one could easily tell what an entity is capable of at any time. Additionally, because it is flat, it is really easy to save and load. 

While, if everything were based on an object, you would have to come up with a [factory][software factory] for everything, and manage serialization for every unique object, which can be a lot to maintain, **especially** if you have duplicate copies of multiple code implementations which may not all be exact clones of each other.

So, why is an [entity system][] better than the typical strategy? It is simpler to manage, conceptualize(in the long run), easier to save and load, requires less maintenance, easier to change policies, and you can change this stuff at runtime!


### How is it useful?
Well, first of all, something that is very important to game development, at least in my opinion, is being able to save and load at all times for anything you make. Repeating a process because something is incomplete or too complicated at the moment is not readily acceptable to me. Spending 2 minutes to get things set up just to test something you'll watch 5 seconds for is not only kinda harmful to development efficiency(think like 3% of what you could be doing).

Because [Entity Systems][entity system] are practically designed to be definable by just a list of attributes, and a few sets of text, they can all be saved at any time at start and in the future.

## Other Questions
Here are the other questions I thought that others might ask themselves, so here's my attempt to convince you that such a system is a good thing!

### Why is this good for me?
I think most of this has been answered above, but pretty much this will make the development process less stressful, cumbersome, and ultimately more fun.

### How will this speed up my development?
* Iteration time is reduced immensely
    Because starting and stopping and changing things has considerably less overhead, 
    changes may be observed faster, and tweaked accordingly with efficiency.
* Artists, programmers, testers, can all test and alter stuff without involving someone like me to put in exceptions for their desired output.
    This means there's more throughput to the project.
    More Throughput means more gets done in less time!

### How much will this system cost us in time?
I would *like* to say not much, but it is a big bang. Most all of it has to be done in order to see something functional happen for this foundation.

Now of course, it can be tweaked as time goes on, but there won't be many features in the core that would ever be added, those really belong in subsystems.

#### What is there to do then?
Become confident in a complete understanding, and start writing code.

#### What code needs to be written?
For the foundation at least:

* Interfaces for SubSystems and components
* Entity definition
* Message System
* YAML reader / writer
* Protobuf reader / writer
* Component and Subsystem managers
* Logging System, which will likely be built on the Message System
* AngelScript loader and basic game loop.

#### What about an actual displayable game?
Lets assume a simple scope

* 2D Graphics Subsystem
* Input Handler Subsystem

For a isometric 2D RTS

* Map component
* Unit component
* Basic game loop script that handles where the player clicked and activating a unit or not.

## A lot to do eh?
I cannot give a time stamp at the moment for how far I expect to get the foundation done, but I do hope as soon as possible. I'm getting rather anxious and excited.

But, having several engines which I've read the source code for, I have a pretty good idea of how to proceed.

[object oriented design]: http://en.wikipedia.org/wiki/Object_oriented_design
[class diagrams]: http://en.wikipedia.org/wiki/Class_diagram
[skyrim]: http://en.wikipedia.org/wiki/The_Elder_Scrolls_V:
[software factory]: http://en.wikipedia.org/wiki/Abstract_factory_pattern
