---
layout: post
title: Coating Solar Cells
date: 2013-09-20 22:40
comments: true
categories: Solar
---

Today I put many hours into assembling the storage platform `solar-cells`.

One of the big driving requirements for this library on my end is that the configuration needs to be able to be different at run time. Passing around types to secure this problem did not seem worth it to me, and going even towards compile flags and preprocessors did not seem worth it either.

I also know of no mergable data structure for functions. Another goal is to reduce continuous boilerplate for users to write.

The solution I went to was a map of `TypeRep`s to `Dynamic`s. It is thus up to the user to provide `Typeable` data structures to put into this map typed aliased as `Context`. Along with that, a  per-type data interface.

<!-- more -->

## Rationalization

It is up to the user to fill in the per-type data structures. It is suggested to make generic functions that represent the pipeline and to either, fill in the data structure for later, or provide the data structure on demand given the type context.

Although partially applied functions is one solution, it won't take into account the possibility for changes such as having to reconnect to the database.

In the case of database reconnection or connecting to another source at runtime (between requests), it would be best to re-use the context to hold connect information and the likes.

Further! For cases like [Riak][], a structure called a _vector clock_ is needed to efficiently resolve and store data that may have forked in the masterless environment. Because this can change during the request, and it is necessary to keep such information during the request, yet without coupling the pipeline to the user logic, this seems like an adequate solution.

## Design

### For the User

Given successful startup, configuration checks, and so on, the storage collection should be prepared for future use, and the `Context` ready to use.

Given a request, they should obtain the `Context` by partial application, or lift the `Context` from whatever monad they are in, or by accessing an `STM` or `IOVar`. *I personally recommend accessing an `STM`.* Then the user must obtain the storage methods for the given types they want to access, via a collection passed by partial application, or by lifting from whatever monad they are in. *Theoretically, an `STM` would be possible to use here, but I don't see why, since it shouldn't change at runtime.*

### Backend Implementors

To help simplify the scary dynamic stuffs in a strict language, I provide a function called `contextWrap`, which takes a function that gets successfully applied to the part of the context for configurations, and also a part to do in the case of no results in the context. `return Nothing` for a `get` operation.

The developer here needs to make a way to delete, get, and put entities *(that satisfy a type class of their choice)* on a given single medium.

## To do

Create storage implementations for

+ File System *Wrapper already ready!*
+ In Memory
+ Riak
+ Redis

## Example

I had to use **NoMonomorphismRestriction**, in case you know why, please let me know. 

```Haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- This example is known to be incomplete...

import qualified Solar.Data.KV as K
import Solar.Storage.FS as FS
import Solar.Storage as St
import Data.Map as Map
import Data.Dynamic as D
import qualified Data.ByteString.Lazy.UTF8 as U
import Text.Read(readMaybe)
import qualified Data.ByteString.Lazy as BL

-- ....

kvToBS :: (Show n, Show r, Show c, Show (d n r c), Show (c' n r c))
       => K.KV n r c d c'
       -> BL.ByteString
kvToBS kv =
    U.fromString $ show kv

kvFromBS :: (Read n, Read r, Read c, Read (d n r c), Read (c' n r c))
         => BL.ByteString
         -> Maybe (K.KV n r c d c')
kvFromBS b = readMaybe $ U.toString $ b

fs = FSMethod
    { fsExt = "txt"
    , fsRead = kvFromBS
    , fsWrite = kvToBS
    }

path = KFilePath "/tmp"
cont = Map.insert (typeOf path) (toDyn path) noContext
```

Going with what's in the playground, using `putFS fs cont kv` now creates a file in `/tmp/Red/Hai.txt`! How cool is that!

Given

```Haskell
type TestKV = K.KV Color Vehicle Ponies Forum K.KVNoCache
```

I can also do `(getFS fs cont ident) :: IO (Maybe (TestKV, Context))` and successfully get my data back out!


[riak]: http://docs.basho.com/riak/latest/
