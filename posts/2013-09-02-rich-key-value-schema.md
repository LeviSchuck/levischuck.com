---
layout: post
title: Rich Key-Value Schema
date: 2013-09-02 12:38
comments: true
categories: Solar
---

## Introduction

In this document, I will be describing my goals / constraints, my plans to fulfill those constraints with a *weak* specification, and reasoning how my *weak* specification fulfills those goals / constraints.

## Goals

+ Simple for arbitrary storage
	* Possible to be within just key-value to string
	* Can be stored as individual files
	* Possible to store in a partially specialized document database
+ Cache friendly
+ Relational outside of manual data specification
+ Graph Queries possible for workers, not meant for real time
+ Friendly for monotonic conflict

<!-- more -->

## Entity Contents

+ Entity Identifier
+ Entity Classes
+ Arbitrary Data / Properties which can be materialized through another schema
+ Last Modified Date
+ Last Content Modified Date
+ Last Relations Modified Date
+ Last Cache Modified Date
+ Invalidated
+ Relational Collection with
	* Relation kind
	* Identifier of remote Entity
	* Remote Entity Classes
	* Relation Direction
		- Both
		- Forward
		- Backward
	* Invalidated
	* Date Added
+ Relational Caches
	* Embedded Entities that are members of the Relational Collection with specifically filtered data from perspectives

## Reasoning


### Arbitrary Storage
What's so special about arbitrary storage? The less we are tied to any particular storage, the more open we are to better storage options in the future. Perhaps something like [Datomic][] could be an attempted platform later.

However! *Arbitrary* as in the data can be anything like document stores such as [Mongo][] is not the focus. The data is still structured and has its own schema! At least, it is **highly** suggested that it has its own schema.

The primary data storage format chosen is [JSON][].
Although it may be entirely possible to also use [Protocol Buffers][Protobuf], which should take less storage space. While writing this document and doing some checks with Haskell, I found [a neat article](http://breaks.for.alienz.org/blog/2013/04/19/protobuf-hackers-at-the-dojo/) that solves this problem! Generics! So perhaps such will be supported, but not enforced. Likely such other implementations will provide a means to get back to the base type, which depends on a JSON + ByteString representation. That is, where the per-type value is a ByteString, and the meta data can be represented within a `ByteString`.

I would look at [msgpack][] if the client in Haskell were not as conflicting with the Haskell Platform's versioning with ByteStrings. It is not ruled out though, but will not be something I'll do for now.

This suggests that each storage option must be able to store byte strings, if not, then [base64][] encoded strings.

### Cache Friendly
Caches primarily act as a map that stores strings of sorts, which is one aspect that supports *arbitrary storage*.

What is more important, is what caches need to function effectively: a version difference. Such can be intuitively represented with a date time. 

For this reason, multiple dates will be included to support differing caches for

* Content Writes
* Relation Writes
* Cache Writes
* Any Writes

Also, since the data can be relational, but only parts might be of interest when requesting a single item, it will be possible to include *partial documents* of other entities which relate to this one. However it is up to the application to supply these when storing. The Cached Entities, which are bundled with the main entity, should be contents that are not sensitive to time. 

For example, on a social network, It does not destroy the experience if, for a non-friend who doesn't have a local more-up-to-date copy in their cache, that an outdated picture or display name is used.

In the end, I'd like to be in a safe situation like [StackOverflow][], where I can depend on things like redis or memcached for such things.

### Relational

I want to separate the content data from the relations. Unlike MySQL, where all of it is put in one record, and relations are pseudo-defined by foreign keys that joins happen on, it is my belief that the content should not be coupled, and conjoined with the relations.

Thus, there is explicit support for relations, primarily directed, which can be handled and reasoned about abstractly.

### Graph Query-able

I am also providing directions on relations as to allow for more complex analysis of data, since not everything is truly bidirectional.

The point of this is so that workers can conclude results asynchronously from requests, and cause side effects, such as events.

A use case of this can be seen where by adding a friend, an analysis may be run which can suggest mutual friends to add.

### Monotonic Conflict Resolution Friendly

One constraint is that you don't want to lose data in the face of many writing data to the same place. It isn't feasible to have a global lock in a masterless environment like [riak][], where a "latest one wins" situation isn't good, especially for relations!

For this reason, about everything contains an **invalidated** entry. As a state machine.. you cannot reverse invalidation, the only way is to add it again as valid. Sure, perhaps at a later time, workers can reconcile things that have such bloat and have *cooled down*. 

## Conclusion

The focus of this structure is going to be primarily agnostic towards which storage engine and storage format used. Though, I would consider it shooting yourself in the foot to try to store multiple storage formats in the same engine.

It will be friendly to store in multiple storages simultaneously.

The meta data is given more importance than the data. 

Next Topic
 ~  indexes, full text search.

An addendum to this schema may be discussed later to support at least some form of indexes. I do not anticipate handling full text search, [Apache Solr][] would handle this better and would be up to the application to submit things to the search database. Solr would also be capable of other indexes, which leads me to suppose that perhaps it should not be my concern with this schema.

This schema, with support such as monotonic conflict resolution, would make highly available, redundant, hot data, able to fork and merge without inconsistent results. For hot data, this removes the global bottleneck at the cost of a local overhead of analyzing the data, and storage space.


[Datomic]: http://www.datomic.com/
[Mongo]: http://www.mongodb.org/
[JSON]: http://json.org/
[Protobuf]: https://developers.google.com/protocol-buffers/docs/overview
[base64]: http://en.wikipedia.org/wiki/Base64
[msgpack]: http://msgpack.org/
[stackoverflow]: http://meta.stackoverflow.com/questions/69164/does-stack-overflow-use-caching-and-if-so-how
[riak]: http://docs.basho.com/riak/latest/
[Apache Solr]: http://en.wikipedia.org/wiki/Apache_Solr