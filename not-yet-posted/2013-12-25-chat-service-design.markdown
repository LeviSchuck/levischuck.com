---
title: What it takes to make an extensible chat service?
---

Now that classes are over for the year, I am
considering what would go into making an extensible
chat API, over [HTTP][], [WebSockets][], and so on, with
varying formats, like [JSON][], [XML][], [BSON][], [MsgPack][], even
the recent [CBOR][]. 

Recently, I got introduced to this whole 
[Domain Driven Design][ddd],
[Command Query Responsibility Segregation][cqrs],
and [Event Sourcing][es] thing.
I'd like to try it.

## Domain

Let's start off with what the users expect or want.
*Most of my contacts like [Skype][], so this will
be the base for opinions.*

### Ubiquitous Language

Here's some terms that we will use for the domain of chat
applications and services.


User
:   
    Users represent individuals that chat with one another.
Chat Session
:   
    An informal exchange between two or more users that continues over some length
    of time.
Log
:   
    A history of past exchanges.
A Contact
:   
    Another user, which the current user has an established relationship with.
Contact Request
:   
    A request sent from a user to another user
    to add the first to the second's contact list.
Block
:   
    Where one user specifies that another user may not establish
    an exchange with another user directly.
Authority
:   
    A logically centralized service which administrates
    user accounts and contact requests.
Message
:   
    An event sent from one user to another,
    whether directly or indirectly, that
    is a fundamental unit of a chat session.

### Contact Management bounded context

Each user has their own list of contacts.

*Having a contact in the user's list allows
one to attempt to establish a chat session
but may not guarantee it. Such is left
up to the Conversation bounded context
to decide in collaboration with this
(Contact Management) bounded context.*

Since an Authority ultimately manages the 
contacts list that each user has,
a user must notify the Authority of
the user's intent.
Also, an Authority server
must notify the user of modifications,
such as in the case of adding from another
machine.
(This sounds like a great chance for
[CQRS][]!)

Contacts can have varying states after
being added, such as being blocked or
removed.
A removal of a contact may later be
reversed by sending another request.
A block too may be reversed, but while
active, the user that activated the block
will ignore any contact requests or
messages sent which are from the
respectively blocked user.

Below, I made a graph to display the general
flow of how a contact becomes a established
on both ends.

<figure>
    <a href="/graphs/contact-states.svg"><img src="/graphs/contact-states.svg" alt="flow graph" /></a>
    <figcaption>A state-like flow graph of how the contact mechanism works.</figcaption>
</figure>

*To reduce the graph's complexity, removal after blocking operations are not shown.*

### Conversation bounded context

The conversation bounded context manages active
and past conversations.
It also manages messages, which are the
fundamental unit of a conversation.

Messages may be textual, or in some other
yet-to-be-determined media.











## Architecture


[FQDN]: http://en.wikipedia.org/wiki/Fully_qualified_domain_name "Fully Qualified Domain Name"
[CBOR]: http://tools.ietf.org/html/rfc7049
[msgpack]: http://msgpack.org/
[bson]: http://bsonspec.org/
[json]: http://www.json.org/
[xml]: http://www.w3.org/XML/
[http]: http://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol
[websockets]: http://en.wikipedia.org/wiki/WebSocket
[cqs]: http://en.wikipedia.org/wiki/Command%E2%80%93query_separation
[cqrs]: http://martinfowler.com/bliki/CQRS.html
[crud]: http://en.wikipedia.org/wiki/Create,_read,_update_and_delete
[ddd]: http://en.wikipedia.org/wiki/Domain-driven_design
[es]: http://msdn.microsoft.com/en-us/library/jj591559.aspx
[skype]: http://www.skype.com/en/