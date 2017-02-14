---
title: PICO 8 the tiny fantasy console
---

## Fantasy console

The thing about consoles is that to be real time, you need to
abide by and respect limitations.
As opposed to using `malloc` and `new` all over the place,
console programming requires preallocation and pools to be
predictable and resilient in a constrained environment.

It looks like [lexaloffle][] decided to make an engine of sorts,
combined with development tools to make things happen!

Welcome, PICO 8! *More than a year late on my part for noticing.*

PICO 8, the program, is an emulator of sorts for a console that
does not exist--hence "fantasy".
It is quite restrictive, but it comes with some pretty neat tools:

* Code editor (with lua)
* Sprite editor
* Map editor
* Sound effect editor
* Music tracker editor

Game code executes a version of [lua][].
As with the restrictive elements of PICO 8, game code is also limited.
It appears that counting of code size in lua is not characters,
but rather AST elements.

![](/images/pico/code.png)

Luckily, you can use (ctrl/cmd)+(A/C/P) to copy and paste the code
to another editor--as 128 pixels wide is quite limiting.

### Tiny demo for a tiny console
If you want to try out [my first cart][spoopy], hit play.
It also has some short tune, though I can't claim much musical affinity.
*However, inputs are ignored, treat it as a video, not a game*

![](/images/pico/spooky.gif)

## Other things

It's got a simple API to write pixels to the screen, though as an
8-bit console of sorts, it certainly is not strong or fast.

![](/images/pico/rendering.gif)

I attempted to make a [distance aided raymarcher][raymarch], turns out that
even when doing ray calculation wrong, you can get interesting results.
However, these results are primarily an artifact of how many steps it
takes to overflow the fractional numbers.

Yes, overflow. Turns out that the PICO 8 uses fixed point math!
It's pretty neat, something I've wanted to try on micro-controllers
and such.
However, such arithmetic miss a significant benefit! Infinity!
That is, instead of overflowing from positive to negative or vice versa,
[IEEE 754 Floating Point][IEEE754], standard floating points we know
and love have a couple magic values that can be occupied.

[lexaloffle]: http://www.lexaloffle.com/
[pico8]: http://www.lexaloffle.com/pico-8.php
[spoopy]: http://www.lexaloffle.com/bbs/?tid=28830
[IEEE754]: https://en.wikipedia.org/wiki/IEEE_floating_point
[raymarch]: http://www.alanzucconi.com/2016/07/01/raymarching/
[undertale]: http://undertale.com
[lua]: https://www.lua.org