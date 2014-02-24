---
layout: post
title: Define, Iterate, Revise the Solar Cells
date: 2013-10-01 10:07
comments: true
categories: Solar
---

While working on the storage API in Haskell, I struggled with getting it to act concise, comprehensible, and yet without making assumptions that would kill context-sensitive storages.

In my adventure, I learned and found some new tools! The `StateT` monad transformer, and implicit parameters! Although the `ReaderT` monad transformer could handle what implicit parameters are used for, I find implicit parameters allow the end user to be more terse.

<!-- more -->

## Define

What is it that I want? I want simple code.

```Haskell
lookupThing t = do
  ?s <- someStorageConstant
  context <- getContextSandbox
  (res, context') <- runKV context $ do
    thing <- getKV t
    return $ case thing of
      Nothing -> Left "Nope, nothin'"
      Just kv -> Right kv
  return res
```


*Note, this is merely a prototype and does not reflect the final code*

The above example could be made even simpler when the top monad has more features, such as the `Yesod` monad, and a further `run*` function which manages the context, storage, and so forth.

What next? I want to be abstract from the storage engine that is being used! Within a small function, who cares if the production storage is `riak` or the testing storage is in memory or on disk? More so, don't we want to take advantage of caching automatically without thinking about it where we use it?

For Key-value storage, the storage options are embarrassingly without limit. To take all the options used in development for every place that uses such functions would be insane! What then can we do?

Each kind that is stored has a provided storage method. But then.. what if the method depends on the environment at run time? Wouldn't you want to simply flip a flag and use another storage on start? Isn't that what other environments give with ease? 

I have a solution! At least in the Haskell pure world. A data structure which, for every kind, stores the individual operations, but can resolve to an intermediate data structure at run time that has only the operations for an individual type. Yes, there's boilerplate here, but at least it is in one spot, and only one spot. A way to simplify it would be to make polymorphic functions, regardless of the precise type, which can be generically applied, such as the following. 

```Haskell
genericFunction :: (Show a) -> a -> String
genericFunction thing = show thing

data OneThing = OneThing (deriving Show)
data AnotherThing = AnotherThing (deriving Show)

data MyFunctions = MyFunctions
  { showOne :: OneThing -> String
  , showAnother :: AnotherThing -> String
  }

currentFunctions = MyFunctions
  { showOne = genericFunction
  , showAnother = genericFunction
  }
```

The concept is then, when `OneThing` is expected, a type class on top of `MyFunctions` for `OneThing` filters `currentFunctions`, which results in the implementation of `genericFunction` for `OneThing`s.

With this in mind, the functions can be chosen at run time, but unlike a dynamic approach, lacking a function will result in a compile error.

Lastly, I need something that can handle future implementations that violate the explicit static assumptions. In other words, I need to handle arbitrary state. My prime example for this is the masterless environment, riak. When you do a `get`, you also get what is called a vector clock, or `VClock`. In riak, each entity that already exists has it's own vector clock. When you update, you need to provide the vector clock so that it can ensure that there are no conflicts. Thus, an arbitrary context is really needed for any request unless the user wants to manually handle vector clocks. Ideally, vector clocks should just be inferred in other languages and ORMs. However, this is not an ORM, which is inherently mutable and involves IO. 

In order to store anything arbitrary in Haskell, it must be `Typeable` and is thus storable as a `Dynamic`. Which isn't that much of a demand on any storage library developer.

## Iterate

Throughout developing `solar-cells`, as well as a proof of concept implementation, `solar-cells-fs`, I rewrote everything about three times.

At first, I just wanted life to be 'simple' enough, make everything dynamic, and if things are not found, run stub functions. However, after contemplation, the guilt got to me. This is not the Haskell way! Any user should be able to know, before they run, if their code is complete and will behave! The compiler is here for a reason, let's use it. *(Though, if only we had ways in meta-land to give specific errors (as API usage errors) when things are found to be incomplete.)*

So, given I started at dynamic, I then tried to find a dependable static way. At first type classes seemed to be the way to go. After some implementation and experimenting, it just wasn't something that can be decided at runtime. Gosh this is a hard problem! 

How to balance between the two? Well, data structures and more type classes! I actually described this in the previous section, but I wanted to give you an idea of how I got here.

Implicit parameters may be thrown out, or have another interface to deal with, for now it is a convenience where the compiler just automagically passes the parameter along.

## Revise

What to do next? Settle this whole implicit parameters deal, I don't like the thought of forcing others to use a horribly undocumented extension, as well as the annotations required for it to work down the chain.

Also, secondary indexes. There's no support! It would be useful and helpful. As well as having an available implementation that uses the other base functions `get`, `put`, `del` and so forth, that implements this in case there is no native support.

Making an arbitrary key-value lookup manually to lookup things like what email matches to a user and so forth is just silly! And really something that shouldn't be concerned with.

By the way, Primary indexes are the actual keys, where as secondary indexes would be like valued attributes, like email, username, url-name, etc., where the actual `KV` lives under a guaranteed unique name / id. 

I hope this was exciting! Plenty more to do, and then next, the Solar Wind broadcasting foundation! *Maybe I might compete with RabbitMQ, who knows.*


## Oh, and.. 

One of the big motivators here is how much time I have spent trying to use [puppet][] with [vagrant][], just so I can have databases prepared, and so on to mirror the *anticipated* production environment… 
Not everyone has 32GB of RAM in one of their machines! Nor do they, in the case like me, have much space on their laptops. Besides, being flexible and having fallbacks is nice, especially in development. You end up mocking plenty of stuff, why not mock everything?

Well, if you're a masochist like me, you'd ***want*** to use your mocks locally to keep development time low and make the write-compile-play-test loop fun again! Of course, the masochist part comes from the amount of thought and development that goes into making all that possible in the first place. My confidence is firm that this will be worth it. Especially in a fun-to-refactor language like Haskell where everything is reusable and compose-able. *You can't say that about actors! (At least not to my knowledge)*





[puppet]: http://puppetlabs.com
[vagrant]: http://www.vagrantup.com