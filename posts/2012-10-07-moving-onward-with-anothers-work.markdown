---
layout: post
title: Moving onward with another's work
date: 2012-10-07 22:04
comments: true
categories: Development
---

My engine development has not been going at the speed I would like,
though I cannot alter the amount of time I can dedicate to it, due
to work, and classes.

Anyway, so I'm involved with another group right now, which I was 
hoping to have my engine design as the **complete** foundation for.
But again, my progression was not satisfiable to me, and possibly
to the others.

So here's what I'm going to do, I'm going to use [slick2d](http://www.slick2d.org/),
and [artemis](http://gamadu.com/artemis/)

## Slick2D
> Slick2D is an easy to use set of tools and utilities wrapped around LWJGL 
> OpenGL bindings to make 2D Java game development easier. 

## Artemis
> Artemis is a high performance Entity System framework for games, written in Java,
> and is a framework to manage entities in a game world. It is inspired by Entity
> Systems are the future of MMORPG blog series by Adam Martin.

<!--more-->

One thing to note though, the pragmatism between Artemis and [my own](/pages/pragmatic-definitions.html)
differ greatly. But, with an [adapter pattern](http://en.wikipedia.org/wiki/Adapter_pattern)
and correct design, I don't think that possibly bringing theirs to my own will
be difficult.

## Differences of Pragmatism with Artemis
### Components
First, Components don't mean the same thing. To Artemis, Components are data
containers, and only data containers. To me, Components are just code that
act on contracted properties.
Artemis's Components are my Contracted Property Tuples.

### Entities
Entities in Artemis are just a number, which happen to be a 
[GUID][], a 128 bit number with no particular meaning other than uniqueness.
For me, an Entity has accessible Attributes, definitions of Properties,
and a single 32-bit unsigned integer.

[guid]: http://en.wikipedia.org/wiki/Globally_unique_identifier "Globally Unique Identifier" 

### Entity Systems
Entity Systems in Artemis is where the actual code lies. They use 
fast mapping so that Artemis Components can be selected fast based on 
an Entity GUID.
For me an Entity System is just the concept of how all the data and code
works together.
Artemis Entity Systems are my Components.

## Slick 2D

So far I'm impressed, it wasn't too hard to get started, once I found an
incomplete series of videos getting stuff started. Additionally, the
documentation isn't half bad. I was able to get most things with ease.

However I do not like the [Tiled](http://www.mapeditor.org/) map editor,
since images, and properties are completely separate and not enforced. 
But for now I'm going to have things set up such that I'll use it for
display purposes at first, and possibly use the Slick2D built-in support
for the Tiled Map Editor's format so I don't have things on screen
navigating on a black abyss.

I'm also getting 2-3K FPS which is nice. 