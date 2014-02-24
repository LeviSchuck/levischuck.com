---
layout: post
title: Facebook theories
date: 2013-08-25 16:50
comments: true
categories: 
---

I was observing the contents on the network with Chrome,
trying to figure out how the giant omnipotent Facebook does it.

Well, it isn't all that complicated, though it still seems
quite hackish in my opinion.

It does give insight into how these work

+ Page generation
+ Messaging
+ Notification
+ Live updates

<!--more-->

> Note, that the following are just theories based on my personal
> observations, and may not represent fact. These will likely become
> invalid in the future, but for now, I hope these can be used as
> a resource to think about. 

## Page Generation

The server is in complete control. HTML is still rendered on the server
side, and then sent within JSON to the client for content rendered after
the initial page request.

This suggests that there is little versioning to do on the client side.

### Benefits

+ New features are not arbitrarily public and can be done
    easily on a user level, without the need of creating
    more public client *builds*. This allows for easier
    silent beta testing. **The clients can also catch any
    errors, report them back, and be given an older stable
    version.**
+ Client is still dumb, less room for bugs by mismatched logic
    between the client and the server.
+ All relevant data is sent over the wire, so there won't be
    need to have additional requests if the server makes
    an incorrect assumption about local cache.

### Hindrances

+ More gateways need to be maintained with regards to the API.
    Though technically what they have currently can be built on
    top of their API.
+ More data has to be sent over the wire, regardless of local
    cache. _(This is a dual problem and solution)_

## Notification, Live Updates, Messaging

> This group is bunched together because they
> all ride on the same mechanism.

It seems that Facebook has implemented a simplex
_(meaning one way)_ transmission line for their updates
to the client. They seem to send a JSON Stream, which is
a long-lived _(60 seconds)_ HTTP request, which uses
`Transfer-Encoding: chunked`. By sending chunks, which
are sections of data prepended by a hexadecimal count of
how many bytes to receive, a chunk-aware XHR library
may read the chunks individually instead of waiting
for the entire request to come through.

Each chunk seems to contain what seems to be a command
to execute, and the data to execute on that command.

Ignoring the `for(;;); ` that prepends each response,
the response that comes when no activity occurs is

```json
for (;;); {"t":"heartbeat"}
{"t":"msg","seq":704,"ms":[{"overlay":{"100000806724235":{"a":2,"ol":0,"s":"push"}},"type":"buddylist_overlay"}]}
{"t":"heartbeat"}
{"t":"continue","seq":704}
```

The above occurs on the channel pulling.

As stated before, there's a command, such as
`heartbeat` and `continue` with other parameters
provided, namely a `seq` parameter, which is
important later. Though, also notice, that
we seem to have data wrapped up for a message
within this. It is not coincidental that the
buddy list got updated during sequence `703`.

Let's take a closer look at it.

```json
{
   "t":"msg",
   "seq":704,
   "ms":[
      {
         "overlay":{
            "100000806724235":{
               "a":2,
               "ol":0,
               "s":"push"
            }
         },
         "type":"buddylist_overlay"
      }
   ]
}
```
We can see that the type, `t`, is a message, `msg`.
It states that it will take place likely in the next
sequence, `seq`, and then the message contents, `ms`,
state that the type is for `buddylist_overlay`.

I can only assume then, that the buddy list overlay
is then notified with the other parameters that are
received, namely `overlay`. I cannot deduce what 
the parameters are inside of that, and it is not of
further interest to me at this time.

On other requests, there seems to be a different
schema, though similar in how it is parsed.

```json
for (;;);{"__ar":1,"payload":{"time":1377473850000,"notifications":{"no_change":1}},"bootloadable":{},"ixData":[]}
```

Or pretty printed and the redundant prefix removed...

```json
{
   "__ar":1,
   "payload":{
      "time":1377473850000,
      "notifications":{
         "no_change":1
      }
   },
   "bootloadable":{

   },
   "ixData":[

   ]
}
```

Now this is interesting. 

The `payload` is especially filled with more
data than I'd want for things like the buddy
list updates. This coincides with my previous
observations of how facebook makes no judgment
on the client's awareness. 

Though I do not know what to make of `__ar`,
`bootloadable`, and `ixData`. I am not interested
enough to investigate that.

So, now that we have two established schemas, 
one for live updates, and one that comes by
client demand, we can say that the application
comes in two tiers of requests.

The primary tier includes the server and the user.
The user is not the same as the client, which is
just a dummy application.

This has not been verified, but to simplify my
observation, let's assume that a click on some
element causes something like

```json
{
   "t":"msg",
   "seq":704,
   "ms":{
      "user":123456,
      "type":"friend_request"
   }
}
```

Let's then suppose that there is a mapping of
the `type`s, be it `friend_request` or
`heartbeat`, which then issues requests
to the server.

Those requests respond with the second schema
and I can only assume that the interested
component would handle what's in the `payload`.

But that's not what is so interesting to me here.

What is interesting is not what I found in the
response contents, but rather the request
headers and the response headers.

Request Query String:
```python
channel:p_123456
seq:755
partition:15
clientid:deadbeef
cb:acli
idle:557
cap:0
mode:stream
format:json
```
I replaced my user ID with `123456` and my client id
with `deadbeef` since it seems to be in hexadecimal.

Let's assume that the `clientid` is similar to a session,
and that Facebook allows multiple sessions. For example,
you can have Facebook open on your desktop and laptop.
They have the same user, but different `clientid`s.

I assume that the channel prefix `p_` signifies that it
relates to a user.

This is also an assumption, that the `partition` is
**calculable** based on the user id and the number of
partitions available. This partition number is also
kept in the cookie. Let's also assume than on
partition topology change, that the user is commanded
to change to another partition, and the value in the
cookie is updated. The partition number may also take
into account locational factors, but for now, let's
assume that spatial information is not taken into
account for the sake of simplicity.

The notions of partitions make sense for a significantly
large system with many concurrent users connected
simultaneously. If there were no sense of partitions,
then each server which sends events to the client will
have to be made aware of every single event, thus the
same as there being only one partition total.

I propose the following logic for channels and partitions.
If the channel does not belong on the requested partition,
then the connection ends immediately with information
that directs the client to the correct partition.

What we have left.. is the importance of the `clientid`
and `seq`. The other values will be disregarded for
analysis.

Every `pull` request ends with a continuation of the
logically next sequence `seq`+1. This suggests that
events will be enqueued on the next sequence between
the disconnect and reconnect.

A client cannot request a previous `seq` entry,
when attempting to do so, it will provide an error
message, and a new `seq` to use which is current
for the given `clientid`. `seq`s are not globally
unique, and are not shared between `clientid`s.

This seems to suggest the following strategy.

![](/graphs/facebook-event1.svg)

Or.. in some ugly haskelly pseudocode
```haskell
-- | Forward declare some functions that figure
-- the things out.
user2Chan :: UserId -> Channel
chan2Partition :: Channel -> IO Partition
-- ^ Partitions might be dynamic, so make it impure
-- and use IO.

data Event = Event
    { evUsers   :: [UserId]
    , evDate    :: UTCTime 
    , evData    :: a
    -- ^ Polymorphic type 'a'
    }

sendEvent :: Event -> IO ()
sendEvent e = do
    eventTuples <- mapM mkTuple (evUsers e)
    mapM sendSingularEvent eventTuples
    where
        mkTuple :: (ToJSON a) 
                => User -> IO (Channel, Partition, a)
        mkTuple u = do
            let chan = user2Chan u
            partition <- chan2Partition chan
            return (chan, partition, evData e)
            -- ^ A reminder to non-haskellers...
            -- return isn't the same as in other
            -- languages. Here, it is wrapping
            -- up the value (a, b, c) into an
            -- IO (a, b, c).

sendSingularEvent :: (ToJSON a) => (Channel, Partition, a) -> IO ()
-- ^ Forward declared function that takes a tuple
-- and delivers it to the relevant server.
```


```Haskell
-- Let's assume now that we are elsewhere in another
-- process, within one single partition.

data Seq = Seq 
    { seqId :: Int
    , seqExpire :: UTCTime
    } 

getClientIdsForChan :: Channel -> IO [ClientId]
getCurrentSeq :: ClientId -> IO Seq
getSeqToSendOn :: Seq -> IO Int
sendSingleEvent :: (UserId, Int, Value) -> IO ()

-- | In this process, the following function gets called
-- to handle requests.
receiveSingularEvent :: (ToJSON a) => (Channel, Partition, a) -> IO ()
receiveSingularEvent (c, _, v) = do
    --                   ^
    --                   |
    -- Since this we assume we are on
    -- the correct partition, let's ignore
    -- the Partition entry in the tuple
    -- we receive.

    clients <- getClientIdsForChan c
    tuples <- mapM mkTuple clients
    mapM sendSingleEvent tuples
    -- ^ Sends the message over the wire
    -- to the final receiver.
    where
        mkTuple client = do
            seq <- getCurrentSeq client
            toSend <- getSeqToSendOn seq
            -- ^ gives the next logical seq id
            -- if the expire times are in the
            -- past or too close.
            return (client, toSend, v)

```

Yeah, that's a lot of code and such, plenty of it
is just theory, and hardly any of it implemented.
But I use haskell, since it's type system tells
the story of what will happen, very easily.

> A note for non-haskellers, `mapM` applies
> stuff to things that respond with **M**onadic
> actions, in this case, an IO action.

The code itself isn't too complicated,
but essentially the cool thing here is that
the topology of Facebook is designed to be scalable.

Overall, Facebook's mentality seems to be...

> If we think they will use the data, send it
> to them. 
> 
> Bandwidth is cheap, latency is not.

The key word there is *latency*. It directly
affects the perceived user experience.
If the client has to expand every single
resource as it comes down and request the
parts it needs, although less bandwidth will
be consumed, that also means that the client
needs to have more logic to assemble the data
and spend its time waiting for more data to
fill the gaps for what it does not know.

Then by this flow of logic, we can say that
if you wish to save more bandwidth, you have
to pay with higher latency.

It would be inconceivable to send the entire
application state to the server to figure
out what to send and what not to send. Also
keeping record of that on the server side
will increase the time spent on the server
to obtain the current state, lazily built
from the client. In the end, keeping such
state on the server, for the sake of bandwidth
is not worth it, and is a pain to even
imagine.


## Conclusion

The server builds everything the client sees.
No features are accidentally sent to the user
for them to dissect and manipulate.

Facebook is scalable, they've overcome that
problem. However, in my humble opinion, at
the cost of what the client can do. 

They use multiple tiers of partitioning to
put new content on the screen of the user.

Facebook has an intuitive system, which
seems to depend more on persistent data,
instead of also live data, to subscribe
to arbitrary elements that happen to
be on screen. Since there is not a duplex,
two way, transmission, such a capability
would be costly to do.

---

This has left me pondering what it means
for my own projects. 