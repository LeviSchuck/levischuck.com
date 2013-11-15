---
title: First time testing, also with FP Complete
---

Today I decided to finally start using tests before
I get any deeper into my [Solar Wind][] project.

Recently, I saw a new framework called [tasty][]
which is meant to replace [test-framework][].
It has [HUnit][], [QuickCheck][], and support for
even more tests like [SmallCheck][].

Although I understand the concepts of unit testing
already, I learned a bit about how to take advantage
of QuickCheck when testing my pure code.
My tests so far aren't very covering, but now I
have some experience to make more with ease.

## Decomposition

At first, I tried following what I saw on
stackoverflow once, where a user suggested
using a flag to make everything vieable for
tests.
This worked, but then I saw why people made
`Internal` modules. It made sense, so I
refactored, and it just felt more comfortable.

## All Green!

Then I got my tests passing!

![](/images/first-tests.png)

I used `exitcode-stdio-1.0`.
Though I slipped up and thought `stdio` was
`studio`. A short face-palm and I learned that
lesson.

## Okay, let's try this FP Complete!

Then came trying to get it working on FP Complete.

*I currently have an academic license, and I might
as well try be the guy who gets the bumps (which I
report) on his open source project, to help the
IDE become a better product.*

### Getting it to even compile

Since [Tasty][] is not in stackage,
but it is in Hackage, I tried to get use the
experimental package feature in the settings.

Tried...

    Hackage: tasty
    Hackage: tasty-hunit
    Hackage: tasty-quickcheck

It complained about not finding `Test.Tasty.QuickCheck`.
This didn't make much sense. I changed the last line
to a manual git checkout and it seemed to function,
though it slowed down setup time.
    
    Git: https://github.com/feuerbach/tasty 791cbefbf8ca531b3b27cb07809a04581d38e1c9 quickcheck

Then I was finally able to use it.
I hope that the creator is willing to contribute
to gettting into stackage and maintaining it.

### Execution target

Then came some immense frustration. How do I get
the project to recognize the test as an executable
target?

I ended up having to edit the `.project-settings.yml`
file manually.
Then I started seeing pinks and reds.
The module wasn't expected to be under the name "Test"
since it was named `Main.hs` in my `tests` directory.

So.. I tried changing it to Test.hs, as well as a rename
from `module Main where` to `module Test where`.
It seemed to be happy, but then Cabal threw a fit.

Cabal wants the module to be Main though!
You apparently can't have a `main-is: Anything.hs` but
in `Anything.hs` not have it be `module Main where`...

## After much fighting between...

*I did send a feature request to have aliases
for the FP Complete IDE.*

Cabal won. But then in order to run the tests,
the drop down for the current executable target is
called "Main".

## But wait..

FP Complete says I have failing tests?
This doesn't make sense...

I expect that a lot of what goes on in the
FP Complete IDE involves GHCI. So I tried
running it there instead of a compiled version.


![](/images/ghci-test-fail.png)

It turned out that concurrency and timing
was the issue--what I tested involved
another thread being spawned and carrying
out a STM transaction.

But the errors didn't go away. I just happened
to test the exact opposite condition.
So I flipped that around, then all green in GHCI!

Commit.. test on FP Complete, oh good! It's
all green! *Well, it doesn't have color support,
but it said all tests passed.*

Oh, I am forgetting something.. The compiled test!

![](/images/compiled-test-fail.png)

Wait.. What now!?

I pondered and then realized that in the
compiled `-threaded` environment, it probably
never got the time to spawn and complete the
transactions I was testing for. *Namely,
declaring thread ownership of a resource.*

I threw in a few `threadDelay 1000`
*which is in microseconds, mind you.*

*EDIT: I've solved this without thread delays now.
I used my `sleepOnSTM` method which listened for a
signal on changing a `Nothing` to a `Just ()`.*

Tada!

Make sure you test your concurrent stuff in both
GHCI and compiled to ensure that you aren't missing
something obvious!

## Conclusions

Using `cpp-flags` and cabal flags to hide nothing
when testing just doesn't feel clean. I find that
making an `Internal` module method satisfies the
needs and seems to be the de-facto standard from
other packages I've seen on Hackage.

[Tasty][] was a good first experience for testing.
When searching for `test-framework` related things,
the [reddit post][reddit] mentioned that the current
`test-framework` project seems rather abandoned.

I found that QuickCheck encourages complete
extraction of logic from IO-like things
as possible.
The ability to combine different tests
like [HUnit][] and [QuickCheck][] seamlessly
to be exciting and encouraging to go into [TDD][].

Tasty also supports [BDD][] with [Hspec][] though
it doesn't really compare much to [Cucumber][].
*I do not have experience with cucumber to say
if it is worth it or not, though a template
haskell version would be pretty radical.*

FP Complete was nice to work with
in that it's faster at notifying you of small mistypes
and type system issues as you are typing.
Though I sent in two feature requests yesterday, a bug
today, and I think two other feature requests as well today.
One related to better testing support for individual
functions as you edited them.

I would really encourage use of it, and if you can,
pay for it.
I really believe in what FP Complete is doing, and
I hope to contribute to the haskell ecosystem for
server and web development.






[tasty]: http://documentup.com/feuerbach/tasty
[solar wind]: /projects/solar-wind.html
[test-framework]: http://hackage.haskell.org/package/test-framework
[hunit]: http://hackage.haskell.org/package/HUnit
[quickcheck]: http://hackage.haskell.org/package/QuickCheck
[smallcheck]: http://hackage.haskell.org/package/smallcheck
[tdd]: http://en.wikipedia.org/wiki/Test-driven_development
[bdd]: http://en.wikipedia.org/wiki/Behavior-driven_development
[hspec]: http://hspec.github.io
[cucumber]: http://cukes.info
[reddit]: http://www.reddit.com/r/haskell/comments/1jr8lb/tasty_a_new_testing_framework_successor_to/