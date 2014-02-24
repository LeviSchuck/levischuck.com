---
layout: post
title: Idea concerning tuples
date: 2012-10-03 21:41
comments: true
categories: Notes
---

So, I had the idea in bed last night concerning how I might make
[properties][property]' [attributes][attribute] easier to manipulate, 
and seem more class-like. The concept takes on the idea of a 
fancy [tuple][] initiated with just an [entity][] ID, and a name that
represents the property.

<!--more-->

I found out, that in `C++`, there are a few interesting properties
that happen to represent numbers.

```cpp
1 //An integral type
'a' //A character type, which is a byte integral
'ab' //An integral type.
'abc' //Another integral type..
'abcd' //Another integral type...
'abcde' //Illegal, will not compile.
"abcd" // char const [5]
```

So, if you notice here, characters, and character sequences within
single quotes are integral types, or at least can be represented 
immediately as an integral type.

What's the importance of this? 

Well.. according to the C++ standards, the only non-type things
that may be used with templates are  

+ integral or enumeration type,
+ pointer to object or pointer to function,
+ lvalue reference to object or lvalue reference to function,
+ pointer to member,

Section 14.3.2/2 says

> Note: A string literal (2.14.5) does not satisfy the requirements
> of any of these categories and thus is not an acceptable
> template-argument.

_Very-Sad-Face_

So, it seems I can't accept arrays either. 

They however can support User Defined literals, though that
requires what is core to this idea, which can be emulated to
an extent.. but **Microsoft is _Seriously_ lacking...**

What I'm excited about is _[Variadic Templates][variadic]_.

# Variadic Templates

These allow you to do some cool things, like creating an 
efficient container for data, such as [tuples][tuple], which
are only emulated using _C-Macros_ in [MSVC][].

So, I got a little [CMake][] [package][c++11det] thanks to a
nice guy that has his own dev blog, and now I'll be able to
eventually do a `#ifndef VARIADIC_SUPPORT` or so, and have
__*limited*__ workarounds for [MSVC][]..

Here's the output from cmake for that.
Mac, Windows and Linux respectively from left to right.

[![][macwinlin]][macwinlin]

# Concept

So, instead of verbosely calling and typing things as
[attributes][attribute] are pulled from the [Entity][]
instance, we can have a typed, but seemingly script-like,
but actually static structure that might look like, 
_within code for Components_

```cpp
auto ent = system::fetch<'Warr'>(id); //Get a Warrior  
```

and then later, assuming we know the numeric order, and we 
defined our own template resolver class, we can say somewhere

```cpp
extern const char p_health[] = "health";
//Define something that we use at compile time
```

and somewhere else

```cpp
template<> resolver<p_health> struct { const int id = 2;}
```

Then, in our code, we can process our entity like such..

```cpp
auto newhealth = property_get<resolver<p_health>::id>(ent);
newhealth *= 4; //Some boost for a spell or something
property_set<resolver<p_health>::id>(ent, newhealth);
```

What we now have is something that is __readable__,
statically optimized for compile time, and almost script-like!

This is entirely theory, I'll see how it goes, but it seems
like this will make things just seem too easy.. maybe.

But let's look at the readability of that last section..

1. We don't care what type health is, as long as it is numeric
	since we will only run numeric operations on it.
2. We say to get the property value
3. But to get that, we use 	a resolver to get the health id,
	which a number, a constant offset for the tuple.
4. We then pass the entity in, it figures out the type
	using more templates behind the scene.
5. We now have the value, we do stuff with it.
6. Same sorta deal, but with setting it, this time we
	pass the new value as a parameter. We might
	eventually do some r-value forwarding. Though I am 
	ignorant on what it is, ultimately.


----

Also, elsewhere in the code, it is defined what the
`'Warr'` type is, very similar to the resolver, except
as a function, not as a struct.

The actual position id's given by the resolver is not
explicitly bound to whatever the entity is set as, 
however, that might be changed once I figure out a 
smarter way to pass things.

Anyway, a [Property][] collection might be defined like such..

```cpp
//Forward declare-ish
template<int n> /*something*/ system::fetch<int>(int);

template<> system::resolve<'Warr'>::t & system::fetch<'Warr'>(int id)
{...}//We put together our tuple and return it.

template<> system::resolve<'Warr'> struct 
{typedef system::tuple<int,float,String,Vector3Int> t;}
```

Again, this is just theory, but from what I've been playing with, 
it will be a rather helpful tool to just not think about types
as much, once they are defined, and possibly bound-able
statically in a [contracting process][property contract].



[macwinlin]: /images/cmake-mac-win-lin.png
[property]: /pages/pragmatic-definitions.html
[attribute]: /pages/pragmatic-definitions.html
[entity]: /pages/pragmatic-definitions.html
[components]: /pages/pragmatic-definitions.html
[tuple]: http://en.wikipedia.org/wiki/Tuple
[variadic]: http://en.wikipedia.org/wiki/Variadic_template
[msvc]: http://en.wikipedia.org/wiki/Visual_C%2B%2B
[cmake]: http://cmake.org/
[c++11det]: http://pageant.ghulbus.eu/?p=664
[property contract]: /pages/pragmatic-definitions.html
