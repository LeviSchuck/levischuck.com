---
layout: post
title: Engine Graph concept
date: 2012-09-30 14:34
comments: true
categories: Notes
tagline: I'd like to get my hands on the guy responsible for this mess. 
---

_I recently moved to octopress so I could also easily use [graphviz](http://www.graphviz.org/)
content in my posts_

[![](/graphs/agnostic.svg)](/graphs/agnostic.svg)

----

I've been spending time in the shower thinking about the engine concepts.

I've realized several holes that I would have to fill in later, but the 
effects of doing so at that time may have been hazardous.

<!--more-->

1. My previous concepts of a negative entity are not strong enough.
2. I have to compromise something in the same area in order to provide
	security and trust
3. Additionally, in order to have synced instantiations without being verbose
	would require having meta entities, that is to say, entities that have
	information about another entity. Said other entities will be what the 
	engine renders, holds information like what sprite is there...
	The meta entity would have enough to construct it, but the small details
	are left up to configuration files, and local game code.
	However, these other entities will only be referenced by id, and their
	attributes will not be broadcasted over the network. 

Meta entities can be thought of as game-concept entities, that hold information
such as

* Health
* State
* Destination Coordinate
* Source Coordinate

There might be a _Commute Percent_.

Note this example, I did not include the _current_ Coordinate. 
It would be changing too often for too many units to be practical.

It's more important that we don't broadcast information that can be reconstructed
or information that we know _would be constructed_ based on the previous states 
and current events. Otherwise, this wouldn't be light on the network and it could
impact the enjoyment of the game play if we don't take this into consideration.

Overall, this design __is not enforced__ because of the _agnostic_ qualities.
However, we need to be able to split the entities apart for what is in the Global
Scope, and within that, what is Broadcasted.

I believe only the following kinds will be broadcasted from Client &rarr; Server

* Global Game-Request
* Global Game-Meta

But note, Request is Client &rarr; Server only, thus the only things that will be 
repeated to __all clients__ is Game-Meta.

In order to support something similar previously, I proposed negative entities. 
But... For efficiency reasons, I needed to split the Global Space to support 
shared, but not broadcasted entities. Additionally, Request Entities needed a 
space.

Thus, a more deeply considered breakdown of the entity ID spectrum is required.

## Entity ID spectrum 

Supposing I'm on a 32 bit mobile device, having the central core object being a
64 bit number results in more computations than is needed. 

So, I spent time in the shower pondering a flexible breakdown of an integer, 32
bits.

Here's what I've come up with

![](/graphs/entity-id-breakdown.svg)

This allows 

* up to 16 modes. _We currently only have 4._
* up to 4096 Actors. _Likely Actor 0 is the actual Server, 
	while all other actors are players._
* up to 65536 Entities per Actor, per mode. 

This seems reasonable.

One other thought I had is that this design technically could support a distributed
web server design for a real time forum. This is something I want, but something
I'll keep in mind later. 

## Real Time Strategy Demo

Now that I'm coming up to where the design leaves the data portion, and to the game
implementation portion, I've been giving some thought as to how things will function
with the design previously mentioned.

The following is engine-side only

### Subsystems needed:

* 2D Display
* RTS Assistant
* Input Handler

### Components Needed

#### 2D Display

* Sprite
* Isometric Grid
* Overlay

#### RTS Assistant

* [A-Star](http://en.wikipedia.org/wiki/A*_search_algorithm) path finding algorithm 
* Spacial Resident Object
* Isometric Map
* Spacial Quadtree Server

#### Input Handler

* Mouse Event
* Keyboard Event
* Application Event

I believe the attributes can be specified at a later time.
