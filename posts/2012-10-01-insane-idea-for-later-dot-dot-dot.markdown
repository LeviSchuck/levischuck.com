---
layout: post
title: Insane idea for later...
date: 2012-10-01 12:29
comments: true
categories: Insane
tagline: while(1) cout << &quot;...&quot;;
mathjax: true
---
So, an idea I had while mostly brain dead last night is rather *dotty*.

But, I figure I should document it in case I should try it later.

This is a completely theoretical concept that may be useful, but it is 
not in the realm where I am willing to attempt for a single game.

It seems to be better for having synchronized load with a ton of data.

<!--more-->
[Entity]: /pages/pragmatic-definitions.html
[Property]: /pages/pragmatic-definitions.html
[Attribute]: /pages/pragmatic-definitions.html
[Component]: /pages/pragmatic-definitions.html

## Preluding Concept

The existing [Entity][]-[Property][]-[Attribute][] structures already have the ability
to be stored in a database instead of locally in memory.

However, when considering a single entity, it is not possible to get all 
the information you need in a single record.
Theoretically, the tables only have a few columns.. 

![](/graphs/entity-tables.svg)

I'm mirroring the design included in the graph before, where Attributes 
are promises, or shared references of values.

I do this because defaults have no reason to be duplicated.
*Especially strings.*

About anything can be represented this way, but may not be efficient
to handle when it comes to the MB in raw data, or even GB.

So, we know that the relationship for an entity is that an entity has
some set of Properties and a set of Attributes. These Properties have 
a **contracted** set of Attributes and types.

> Note: It is Illegal for two properties to use the same Attribute with
> different types. So we will not consider such cases.


[materialized views]: http://en.wikipedia.org/wiki/Materialized_view
[views]: http://en.wikipedia.org/wiki/View_(database)

# Concept

But, the problem with the above structure is not it's ability to store.
It is generic enough to support it.
It faces efficiency problems when put into a database with
arbitrarily large amounts of data. Also, when queried, Entities must 
parse one row for every attribute. For sufficiently complex Entities,
there is one join involved, and the data can be scattered all over, 
making retrieval times potentially detrimentally slow with big data sets.

Usually [views][], or [materialized views][] are created in typical 
databases to hide the joins or to have an existing known set of data
quickly accessible.

However, [views][] and [materialized views][] only work when the data
structure is already known.

If we want to replicate the usefulness of tables, we'll need to consider
a few things. 

Let's consider the facts: 

1. We know that only a single Entity by a given ID will exist.
2. We know that all Entities with the same set of Properties will have
	the same exact amount of Attributes with the same types.
3. We know that an Entity's Property set **is fluid**, meaning that it
	can change at any time
4. We know that [components][component] will be the primary consumer of selecting 
	entities from the database and have expectations for what
	Attributes exist.

Let's consider what we desire:

+ Fast Selection of an Entity for a [Component][]
+ Fast Selection of all relevant Attributes for a Component
+ Ability to change Properties at runtime, such as when new components
	may register an existing property with additional attributes

We now move on to determining a solution by using tables that have all
attributes associated with an entity on a single row. 

> Note: we previously stated that the structure for views must be
> known at creation time.

[crc]: http://en.wikipedia.org/wiki/Cyclic_redundancy_check

# Solution

What I propose is that tables are created for each set of Properties.

_Also, because strings are costly to concatenate, the table names should
be fixed in length, so a template that queries can just have a portion
rewritten._

Fixed length information based on variable length information can only
be done with a hash. I plan to use [CRC32][CRC], it's short, and can be
dependable. It's a 32 bit integer, which can be represented in hex.

Suppose we have an entity with the following properties

+ Apples
+ Cheese
+ Bananas

> Note: I put these out of order for a reason

We want an Entity's Properties to be considered as a set, and that the
set has a unique hash no matter what order the Properties have been added.

Thus we sort the list before calculating the hash, which is based on the
names concatenated to each other.

The CRC32 of `ApplesBananasCheese` is `0x56A8B8DB` x is a valid start
character of a table, thus we can name the table for Entities which have
this unique set of Properties `x56A8B8DB`. The columns consisting of the
first being the Entity ID, an integer. The columns following are named an
_escaped_ rendition of the Attribute names, that is to say, Attribute names
which are in a suitable database column name. 

![](/graphs/entity-custom-table.svg)

> Note: We'll consider escaped Attribute names that happen to match, but
> are derived from two different Attribute names to be Illegal, and such
> should be wisely decided beforehand. Also, Attribute names in the
> database are likely case insensitive.

Having such a table defined with the method above fulfills _Fast Selection
of all relevant Attributes for a Component_. However, we need to know which
table to look in.

Though there is one more thing to consider, which brings views into necessity.
A Component should not need to go through indirection to fetch each entity
that happens to have the properties it cares about. 
We already know that a subset of some Properties contain all the
attributes of that set of Properties. Thus if a component only cared about the
properties `Apples, Bananas`, then it can calculate the CRC once and use that
each time, thus string processing is brought to a minimum on the client side.

However, this means that we'll need a view which contains a unions of all
tables which contain these properties.

Therefore, we should create a view called `vB07B9BFA`, _B07B9BFA_ being the 
CRC of `ApplesBananas`.

![](/graphs/entity-custom-table2.svg)

> For readability reasons, the names have not been CRC'd

So, components will actually query views, which are a subset, _though
may not be a proper subset_ of the existing tables. However, this way,
the entry point for a component is simple and not complex.

This fulfills desire _Fast Selection of an Entity for a Component_.

Now for the last desire, _Ability to change Properties at runtime_...

Let's establish that a few meta tables exist, defining what attributes
properties have, their defaults, and their component id's. There also 
exists a table defining for each component, what properties exist, and
an ID for a _Property Template_. Also, there's a table defining all views
and tables which a Property is included in.

A Property Template is the Component's ideal view of a property, listing
it's attributes by name, their types and defaults.

When an augment occurs, the components are reconsidered in sum to find
the union of all Attributes for a given Property, and columns are added,
never removed. We cannot remove columns when a component is unregistered
automatically because component unregistration may happen at any time, but
we may still value the data later if the same component is registered once
more. 

Additionally, views will be reformulated.

We may have a `clean` or `vacuum` command which will remove columns if we
as administrators plan to drop such information permanently.

> Until then, all new entries will have `null` values.

This allows us to safely augment our tables without affecting live
or existing components.

Because we have views, our entities will only exist in **One Table**, 
however, they are accessible in **multiple views**.

All Tables have the Entity Column indexed ascending. 

Meta Tables have about every column indexed ascending, except for 
further meta information, such as the type of Attribute.

----

It is important to notice however, that it is possible for the
count of the powerset of Properties to exist in Tables and Views.

$\begin{equation}
	2^{\parallel Properties \parallel}
	\end{equation}$

This won't be much of a problem since most tables are sparse, and
combination of properties are usually related. It leaves up to the
database to be optimal with the vast data. 

# Done

Wow, this was amazingly long. And it took 4 hours to type out.

It has been plaguing my mind all day, exploding to get out.
Maybe I can finally get some rest and focus on work, and school
stuff...

I'm quite satisfied though that I was able to _(to me)_ coherently
type this all out.