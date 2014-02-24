---
layout: post
title: RTS Map thoughts
date: 2012-10-07 22:39
comments: true
categories: Development RTS
---

I spent another shower session pondering how I can make an efficient
2D RTS work out, that is based on isometric tiles.

I've also been reading some of the source code for [fheroes2](http://sourceforge.net/projects/fheroes2/)
which gives me the perspective of how others developed a game they
already knew all the specifications and requirements for, since there
existed another implementation.

It's not a real time strategy, but what I was most interested in was
how they defined the map and possibly handled sprite animations.

<!--more-->

I never got to the animations, but the tiles were interesting anyway.
I also pondered how [A*](http://en.wikipedia.org/wiki/A*_search_algorithm)
will fit in effectively. It seems it would be best to consider an
on-demand heuristic with some sort of desired calculation length
and a means of memory such that we don't get put into a loop if things
don't work out for the heuristic approaches. However, I have to consider
that in the memory, there will be a _Fuzzy_ flag, meaning that the 
position previously considered taken, or bad, might be available again.

Such is the case if you have multiple objects moving in a cluster, 
the unit might be surrounded, and although they as a group are moving
towards a similar destination, the unit may be surrounded and the only
way forward is the spaces that others free.

Regarding maps, the idea I have is that there will be a quadtree of
sectors, such that locations globally can be found in logarithmic time
while possibly being sparse and expandable.

These sectors will be the size of `8x8` tiles. Regarding spacial qualities
I'm considering that a tile might have three modes of spacial reservation.

1. Entire Tile
2. Tile is split into 4 subspaces
3. Tile is split into 9 subspaces

Entire Tiles for example would be for a large mobile unit, or a tower or 
other structure.

4 subspaces would be for when you have units moving together and to ease
movement of small units in groups.

9 subspaces would be when you have a central resource, such as a tree, 
where it takes up a central space, and spaces around can be considered 
as 9ths or 4ths when it comes to harvesting.

However, the 9 subspaces might just be considered 4 subspaces but not 
possible to take up the entire space with a unit.

----

## What have I done so far? 

I've played around and I've made a means to draw, and drag a map, so 
I'm beginning to understand the means of Input handling with Slick2D.
I rather like it, and my own might follow it's model. 

I've made classes for Animation Data, Animation Sources, Animation 
Instances, so now I can grab a sprite sheet, which Slick2D can load
I can pass it to an Animation Source, define some data, and the source
can keep track of it, then for a given _unit_ I can `tick` the 
Animation Instance, so that it updates frame by frame, and have
a _Volatile Image_ selected and drawn at runtime.

I specify **Volatile** because if it's a dynamically generated Image, 
meant _only_ for that frame, and to be destroyed after rendering.

So I also made a Volatile Image wrapper, which Inherits Slick2D's 
`Image` class.


## What to do next?

Get a `Visual Unit` class going, which has the ability for different
directions if it is a mobile Unit. 

Get a controllable unit class of sorts existing, so that I can give
commands to a visual unit to move somewhere, change it's animation
states automatically, and so forth, which will be important to actually
making a game unit come alive. 

----

_Aside..._

I'm liking my Java experience right now considerably better than I had
over a year ago. My only complain tis that Eclipse generates over a 
gigabyte of garbage every 5 minutes! And When you use 16GB of RAM, 
and your Operating System _OS X_ sucks at global memory management,
you keep having to run `purge` so that it flushes out all that crap.

And this is with Eclipse minimized by the way. This same kind of activity
is what really turned me off from Net Beans.
