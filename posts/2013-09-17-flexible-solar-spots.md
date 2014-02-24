---
layout: post
title: Flexible Solar Spots
date: 2013-09-17 09:43
comments: true
categories: Solar
---

## Data, set, go!

Previously, I was developing a *key-value* storage structure, based on [aeson][], though I realized that the users of it *(especially myself)* would have to constantly deserialize and re-serialize every time they want to access and send the data back over.
In my attempt to be *general*, **I failed** to be *general*. Then.. I looked into making a haskell-style polymorphic data structure! 

It turns out to work quite well when testing in my playground!

<!-- more -->

## Generalness

First off, I tried to not bring in that many dependencies. Unfortunately, [text][] and [time][] have several, but all are at least given in the platform. 

I provide instances of [Show][], [Read][], to support even inefficient storage, but also helpful for debugging.
Also provided is a [Typeable][] instance, meaning that in the future, a library that uses [Dynamic][] would be able to store or interface with such instances. 

[Generics][] is also brought in so that libraries such as [cereal][] can take advantage of it and support serialization.

## Plans

Release `solar-spot` when the first iteration of the *solar* ecosystem is ready.

Create accompanying libraries like

+ `solar-spot-cereal` *(Already ready!)*
+ `solar-spot-aeson`
+ `solar-spot-msgpack`

And more.. 

### Querying

I've had a few ideas for this, utilizing [pipes][], and also some benefits that [dynamic][] can bring into the ecosystem.

More details will be provided on my thoughts after I get the storage platform ready to try it.


## Example

Note, that the language extensions at the top are mainly there so that many of the deriving features will work. They are not necessary in order to use the more advanced features of the `KV` data structure.

```Haskell
{-# LANGUAGE DeriveDataTypeable #-}
module KV2 where
    
import qualified Solar.Data.KV as K
import System.IO.Unsafe(unsafePerformIO)
import Data.Time.Clock
import Data.Text(pack)
import Data.Typeable
import GHC.Generics as G
    
data Color = Red | Green | Blue | Yellow
    deriving (Show, Read, Typeable)
data Vehicle = Car | Truck | Semi | Van
    deriving (Show, Read, Typeable)
data Ponies = Fluttershy | TwilightSparkle | Rarity | Applejack | PinkiePie | RainbowDash
    deriving (Show, Read, Typeable)
data Forum n r c = Forum r
    deriving (Show, Typeable)
    
    
    
time = unsafePerformIO $ getCurrentTime
    
ident = K.KVIdentifier Red (pack "Hai")
ident2 = K.KVIdentifier Blue (pack "Potatoes")
arel = K.KVLink ident2 [Car, Van] [Rarity] K.In time False
met = K.KVMeta ident [RainbowDash, Fluttershy] [arel] time time time False
kv = K.KV met (Forum Truck) K.kvNoCache
```

I provide a `kvNoCache` function that will polymorphically fill the hole in the `KV` data structure when you don't want to store any caches. It is up to you to also provide type class instances of the data contents *(here, `Forum r`)* and the caches if you want to store them or render them in some way.

This example for anticipated `solar-spot-0.1.0.0` shows the basics of just putting in meta data. The actual data contents can be whatever you want. 

I used ponies because they make life more colorful.





[aeson]: http://hackage.haskell.org/package/aeson
[time]: http://hackage.haskell.org/package/time
[text]: http://hackage.haskell.org/package/text
[syb]: http://hackage.haskell.org/package/syb
[cereal]: http://hackage.haskell.org/package/cereal
[pipes]: http://hackage.haskell.org/package/pipes
[show]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t:Show
[read]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t:Read
[typeable]: http://hackage.haskell.org/packages/archive/base/4.6.0.1/doc/html/Data-Typeable.html#t:Typeable
[dynamic]: http://hackage.haskell.org/packages/archive/base/4.6.0.1/doc/html/Data-Dynamic.html
[generics]: http://www.haskell.org/haskellwiki/GHC.Generics