---
layout: post
title: Microsoft doesn't care for developers.
date: 2012-10-12 13:15
comments: true
categories: Comments
---
Let me clarify, **Especially developers that want to use other platforms**
with ease of portability.

There's a lot of bogus positive light floating around Microsoft and their
work on being the latest and greatest and most stable technology to work with.

I [reference Apache's table][apache c++] for support of C++11 _formerly called
C++0x_, the _new_ standard which other compilers, like [Clang][], [GCC][], and
even [Intel][] have been racing to complete.

[MSVC][] however is behind the times. In some cases, such as [variadic templates][],
GCC has had it since 2007. GCC is open source, free, and on all platforms.
MSVC is none of those. They have a team, with a **Genius**, who goes by _STL_.
I'm not mocking him at all, I've watched videos of him speaking, it's clear that he's
capable, and so I would assume his team is too. But it seems _Microsoft_ has it's
priorities on C# and friends with their .NET framework.

I use the .NET framework, version 2.0 at work. It's dependable, and it's great.
The language features around are rather helpful. But, for those that want to have
a cross platform, dependable, efficient, application with a faster development time,
C++11 really helps there. But, MSVC doesn't care. User defined literals? Nope.
Range-based for loops? _Only in our latest and greatest MSVC, which you have to pay for._

I'm considering just using codeblocks on windows, this is just disgusting, I want to
write clean, efficient code, where I am in full control! A lot of people that 
put MSVC under a welcoming light don't know what they are missing out on! 

C++11 is beautiful, but Microsoft resorts to hacks just to get their [Tuple][] support
existing, within a limit.. Which is adjustable, by default at 5, and increases compile
time immensely because it is after all a hack using preprocessors and such...

So, what am I limited to with my MSVC 10?

+ v2.0 of [Rvalue][] references partially.
+ `static_assert`
+ `auto` type _This is nice_
+ Trailing return types
+ Lambdas v1.0 partially _This is nice_
+ Right Angle brackets
+ Extern templates
+ `nullptr` _As a bug fix, because they didn't think it was important at first._
+ Strongly typed enums, partially.
+ Local and unnamed types as template arguments
+ exception_ptr
+ `long long`

This is like a convenience store compared to the super market that is C++11.



[apache c++]: http://wiki.apache.org/stdcxx/C%2B%2B0xCompilerSupport
[gcc]: http://gcc.gnu.org/projects/cxx0x.html
[clang]: http://clang.llvm.org/cxx_status.html
[msvc]: http://blogs.msdn.com/b/vcblog/archive/2011/09/12/10209291.aspx
[intel]: http://software.intel.com/en-us/articles/c0x-features-supported-by-intel-c-compiler/

[variadic templates]: http://en.wikipedia.org/wiki/Variadic_templates
[tuple]: http://en.wikipedia.org/wiki/C%2B%2B11#Tuple_types
[Rvalue]: http://en.wikipedia.org/wiki/C%2B%2B11#Rvalue_references_and_move_constructors
