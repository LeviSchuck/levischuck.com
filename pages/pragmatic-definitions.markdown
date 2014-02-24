---
layout: page
title: Pragmatic Definitions
date: 2012-10-01 21:29
comments: true
sharing: true
footer: true
indexer: true
---

Here's a list of definitions for terms that I use that may
not be consistent with how others use them, or define them.

## Entity System
Encompasses all of the following

![](/graphs/entity-system.svg)

## Entity

A Positive, unique number that pertains to an abstract existence of _something_.

That _something_ is defined by Properties and Attributes.

### Instance
Consists of an Entity ID, an aggregation of Properties, and a collection of attributes.

![](/graphs/entity.svg)

## Property
A textual name that defines what _something_ is. 

### Instance
Consists of the Property name, a set of all Entities that have this Property.

![](/graphs/property.svg)

### Contract
A Component Contracts that a Property has a set of Attributes with certain names and respective types.

![](/graphs/property-contract.svg)

## Attribute
A [Key-Value Pair](http://en.wikipedia.org/wiki/Attribute%E2%80%93value_pair).

Where the Key is a textual String, the Value is an arbitrary value of _some_ type.

### Instance
Raw instances do not duplicate if the same value exists somewhere else if not under the context of a Property Contract.

![](/graphs/attribute.svg)

## Component
A Component is some code that acts on Entities that contain the subset of Properties which the Component has registered interest in.

### Instance
The Instances register Property Contracts so that the Attributes which the component is interested in conform to existence and have some default value.

![](/graphs/attribute.svg)

## System
A collection of related Components.

Components interact through the outside world through a System, or Subsystem. 
Such Systems contain libraries, assistants, or wrappers which interact with the 
outside world.

## Library
A discrete collection of related code that computes and or interacts with the user.

## Assistants
A collection of routines which are optimized for calculation of specific things within the same topic.

## Wrappers
Holds commands and or interactions with an external library with minimal code between, acts as an abstraction.