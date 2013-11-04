---
title: Review of Clay, a Haskell CSS EDSL
---

So, I recently moved my [resume](/resume.html)'s css to [Clay][].
Clay is an EDSL, Embedded Domain Specific Language, for CSS and
allows for something similar to CSS preprocessing.

## My experience

I hit several annoyances, one of which was that `Size a`
is of the `Num` typeclass but.. does not implement the times
operator... Instead, it gives a runtime error. Seriously?

Secondly, there's no `inch` specified! So I had to use the
relative standard for inches being 72pts per inch.

Third, They have a typeclass called `None`, which is nice so you can
say things like `textDecoration none`, but when you want to
center? You have to do this ugly chain of
`textAlign $ alignSide sideCenter`. The same applies to floats
and clears, though with less chaining.

Fourth, they have refinements, like `:hover` and `:before`
pseudo-classes. But they do not implement monoid like other elements.
Normally you can try to do things like..

    (header <> footer) ? do
        color white

and it would come out like

    header, footer {
        color: white;
    }

But this does not seem to apply to things like `hover` and `active`,
which results in literal duplication in the final result if you
use a variable.

    a ? do
        let hv = do
            color white
        hover & hv
        active & hv

and that comes out as the unoptimal

    a:hover {
        color: white;
    }
    a:active {
        color: white;
    }

instead of

    a:hover, a:active {
        color: white;
    }


Fifth, colors like `white` are always rendered into `rgb(255,255,255)`.
This gets annoying to read, but also, in compact mode, they stay that way!
Instead of going to a safe `#FFFFFF` or even further `#FFF`, the
only real difference that I can tell is the removal of white space.

Sixth, margins can only have absolute sizes. The CSS I was coming from
used percents, or relative sizes. In other words, there was no way to
legally express `margin-left: 17%` within the existing EDSL. Due to
this, I had to make the anticipated body width a parameter to my
rules, and due to `Size a` not **really** implementing the `Num` class,
I had to reconstruct my `pt` representation. To me, this forces me to
either know or assume too much about the parameter's meaning in type.

Seventh, some of the constructors like `Size` were not made available
so I could not define my own true `inch` as `"in"`. I was surprised
that Clay seemed to be restrictive in the kind of extensibility that
anyone would just expect. 

## Unexperienced observations

I did not use any of the color functions yet, but compared to
[SASS's color functions][sass function], Clay feels clunky and
incomplete. 

There also does not seem to be much depth to CSS3 rules
that also provide what [compass][] does. 


## Recommendations

For CSS3, the range of which
vendor-prefixed entries would also be best to go into the
`Config`, like `pretty` and `compact`, though best with the
config being a monoid so that you can do
`compact <> prefixed [ms, moz, webkit]`.

I'd like it if clay were broken into multiple packages,
like a `clay-core` with the fundamental monads and structures;
`clay-tags` for things like `blockquote` and `p`;
`clay-color` for the color list and functions;
`clay-vendor` for CSS3 where vendor prefixes are provided
automatically, depending on the configuration;
and a `clay-render` which has its own functions for
taking a `Css` and outputting `Text` or whatever format.

The ability to take a resource at runtime and embed in the CSS,
like an icon in base64 inlined would also be cool.

Looking for exact-duplicate rule bodies and merging the
rule predicates would be an easy step to do in the optimization.
Optimize by transforming known multi-input rules like `margin: x x x x;`
into `margin: x`. In general, there's a lot of work to be done
in the optimization / compact rendering.

Also, some more font families, like `arial`, not just `sansSerif`.

Also, re-export functions or operators like `(<>)` from `Data.Monoid`
since they would be commonly used.

## Final words

Clay is really cool, it is a decent EDSL, there are not surprises to
a moderately experienced Haskeller.
It has a lot of room to grow, and I wish I had more time
so I could contribute to it.


[clay]: http://fvisser.nl/clay/
[sass function]: http://sass-lang.com/documentation/Sass/Script/Functions.html
[compass]: http://compass-style.org/reference/compass/css3/