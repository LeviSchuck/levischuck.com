---
layout: post
title: Plane Ramblings
description: Thoughts I had while on a return trip on a plane
category: Notes
tags: [notes, thoughts, engine, design]
tagline: Let me remind all citizens of the dangers of magical thinking.
comments: true
---


So I was on an airplane, reading some technical pdfs, such as how Intel recommends doing engine design and so forth. I also include my thoughts about my development of the engine.

*Note*: These are raw notes, something I wrote down which I understood at the time of writing.
You might get an insight to what I'm thinking, but I guarantee you, this is very partial data.
<!--more-->

---

### Consider a dispatcher with priorities
Systems are their own task which gives to a scheduler subtasks.

* Considering each master task is function decomposition
* Considering subtasks that manipulate data decomposition

If we have task objects, a scheduler can call virtual functions in task objects.
Task objects can be an interface or an abstract class.

### Entity Change Messages

Systems Listen for specific names of attributes

*Note*: These names are exchanged for integers from the global registry on name registration. 
Messages will not carry a string for the name for comparison purposes, but they might (a constant reference) for debug purposes.
There will only be integer comparison, we will be doing this often, let's make this fast.

Likely the dispatcher will have a hash table of the ID Mod hash stride, containing a pair of the ID and an observer pointer or reference.

Furthermore, systems don't get notified of the change contents. They are just handed an entity ID, which is then put into a dirty hash set. Said data structure will be read at the preprocessing step.

Fixed length data will be duplicated with interested systems.

* We get cache friendly results
* Better vectorization since we handle the data within component systems

Attributes are replaced atomically. Attributes are ref-counted.

For Variant-length data, like strings or blobs, we don't want to duplicate that kind of data, since it can lead to some fragmentation. For the component system that is processing said data, we take partial ownership for that step. We grab the reference every execution phase, and then release.

If the attribute were replaced during this time with a different value, we get sole ownership, and it is destroyed when we release automatically. We do not have to worry about the lifetime of the object, or it being destroyed, or altered in a way that would cause a segmentation fault.

---

### Things to do

Entity Manager

 - Has known max count (initialized to 0)
 - Has a LIFO Queue for recycling free IDs
 
{Section unfinished}

---

Consider as well a static definitions table.
* Used for entity definitions, game templates, etc. 
	For example, a tile, or a certain unit.
* Would be used for unit class identification so data is not duplicated for each entity, such as the picture that is used for it's animation.
* Defined by game config files, be it protobuf or YAML

Subject-Observer pattern

> This seems like a good idea if we want to thin out the channels observers listen to..
>
> **Channels**
>
> By having channels, we can filter out stuff that is completely internal for syncing game states, which should not be done over the network.

---

One problem though, is that if we did broadcast the entity attributes, it assumes a Master-Master relationship. For later things, like physics, this will clearly not do, since latency and syncing the information is not feasible.

It depends on immediate consistency, or eventual consistency.

Perhaps it is not a good idea to broadcast the raw entity attribute changes. Rather, actions that cause them to be nearly consistent, along with a list of entities to periodically compare between clients with an owner listed to resolve who's right.

An owner will be needed at runtime.
