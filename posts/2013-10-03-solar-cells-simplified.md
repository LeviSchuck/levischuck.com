---
layout: post
title: Solar Cells simplified with Monad Transformers
date: 2013-10-03 11:31
comments: true
categories: Solar
---

After some work since the last post, I wanted to simplify and make room for easier optimization.

Further, I found something really cool! The Reader-Writer-State Monad Transformer! Or just [RWST][].

After looking into it, the Writer part is actually helpful, to the end user anyway. If coupled with pipes, and a sink that `tell`s the values, this could make for easily comprehensible, but sophisticated querying.

<!-- more -->
## What's a reader monad?
Suppose you have a constant that you'd like to pass around to your functions in a language like C. You might use global state, or you could pass this constant to each function that uses it. That function you pass it to might pass that constant to sub functions and so on. The *constant*-ness is important here. Since there is no plan to write over the value, we only "read" by `ask`ing the value at any point in the monad. This is fundamental concept of the reader monad.

In Solar Cells, I previously tried to use implicit parameters, but I find that the reader monad for the functions passed at runtime to be cleaner.

## What's a writer monad?
Well, given a *monoid* type, in other words, a structure which has an **empty** constant, and a way to **append** or **concat** values to an existing value of this type, you can `tell` values as they come in the computation. In a mutable language, one might pass around the structure's reference, like a tree map for example. Then as the computation continues, that tree map is modified, added to, and so forth. The Writer monad is like that, except it only monotonically adds values, and you don't have to pass the reference around all the time. Thus, for a writer monad which acts on a set, obviously, you can add to the set by telling a new set of one item, which by the monoid concepts unions them together. You may never access the existing value in the computation.

## What's a state monad?
Given an initial state, you start a computation which can change the state. Similar to a [state machine][], computation may depend on the current state and the results of the computation can and likely will change the state. Thus we can see, computation must be able to `get` the current state, and when a computation `put`s a new state, all further computations that depend on the state may and likely will act differently.

Generally you don't have state when accessing things, but in the case of riak's [vector clocks][], you need to keep track of these for when you update existing values. 

## What's a monad transformer?
Given a base monad, such as `IO` or `STM` (for Software Transactional Memory), or even the `Identity` monad, you can extend the features of another monad on top of the base monad. In the case of a reader transformer, you can do both `IO` actions and `ask` for the constant value within the monad. You access the base monad by `lift`ing the action into the current monad.

## Development so far

With the help of the [transformers][] library, *no, not the big giant immature robots*, I found my codebase simplify and become easier to understand and tinker with. 

I do not yet have much example code, but I have been diligent on writing documentation in the [repository][]. I plan to add more and eventually follow the [pipes tutorial][]'s standard of usage in the package.

Next up, I plan to add basic secondary indexes support. For now, I only plan to support exact matches based on strings. I may add more later, such as ranges or starts-with, but for now, exact matches are sufficient.

### Later
Once I get Solar Cells settled down, I'll head on over and develop Solar Wind, a broadcasting / queue framework. School is taking its load on me, but luckily right now is a low in the cycles of all my classes for assignments and projects.




[RWST]: http://hackage.haskell.org/package/transformers-0.2.2.0/docs/Control-Monad-Trans-RWS-Lazy.html

[state machine]: http://en.wikipedia.org/wiki/Finite-state_machine
[vector clocks]: http://docs.basho.com/riak/latest/theory/concepts/Vector-Clocks/
[transformers]: http://hackage.haskell.org/package/transformers
[repository]: https://github.com/Cordite-Studios/solar/tree/master/solar-cells
[pipes tutorial]: http://hackage.haskell.org/package/pipes-4.0.0/docs/Pipes-Tutorial.html