---
layout: post
title: Xcode gotcha
description: "A little gotcha that I got while trying to use xcode"
category: Setup 
tags: [environment, mac, xcode, library, include, angelscript, POCO]
tagline: Another Successful procedure.
comments: true
---


## Xcode Paths
One thing I found out when using [brew][] is that it installs to the `/usr/local/` directory. That's fine and all, but utilities and libraries aren't found by [Xcode][] even though they are when using [GNU make][] in the normal terminal.

It turns out the paths are not loaded into xcode for what xcode has available. My case was that it was unable to find [Graphviz][] `dot` command, which is used with doxygen for generating documentation. 

## Solution

```bash
sudo ln -s /usr/local /opt/local
```

This does a symbolic link so that anything that happens in `/usr/local` the same appears under `/opt/local`. It is essentially an alias in the file system.
<!--more-->

## Angelscript
Also, it seems that the provided [Xcode][] [Angelscript][] project is a failure. It tries to compile with [MSVC][] semantics instead of [LLVM][].
Mac uses the latter, while windows uses the former. Totally different compilers with totally different designs.
It also uses the __Wrong__ build target SDK, which does not exist on the last __three__ Mac SDKs with [Xcode][].
## Solution
Go into the `sdk/angelscript/projects/gnuc macosx` directory in the terminal, run `make`. Then `sudo make install`

This should set things up so you can link to it properly.

## Poco Foundation
Turns out I was decieved in that I'm building a static library on the other platforms. On Xcode, my cmake generated project builds both static, and dynamic libraries. Static libraries __Do Not__ complain if you don't link in something. It will just cascade to the final executable or dynamic / shared library.

I got a nice thanks to [Xcode][] telling me that it could not find the references to the _reference counted_ class. Turns out I had to link in `PocoFoundation`

Because there are also _Debug_ and _Release_ libraries of it, I'll have to edit my [cmake][] project to use the right one. It currently compiles with the _Release_ version, however _Debug_ builds and _Release_ builds in [LLVM][] and [MSVC][] don't play well together at __Runtime__. 

----

Anyway, Thanks [Xcode][], thanks for all the fish.
I won't be saying *So long,* though. 

[brew]: http://mxcl.github.com/homebrew/
[Xcode]: https://developer.apple.com/xcode/
[gnu make]: http://www.gnu.org/software/make/
[graphviz]: http://www.graphviz.org/
[angelscript]: http://www.angelcode.com/angelscript/downloads.html
[msvc]: http://en.wikipedia.org/wiki/Visual_C%2B%2B "Microsoft Visual C++ Compiler"
[llvm]: http://clang.llvm.org/ "Low Level Virtual Machine"
[cmake]: http://www.cmake.org/cmake/help/documentation.html