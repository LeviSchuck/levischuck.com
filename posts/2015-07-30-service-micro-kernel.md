---
title: Service Micro Kernel
---

Many of the services and products we develop today utilize other specialized services and codebases, allowing us to develop faster and let our customers be successful.

## Fragmented foundations
However, the way we use other codebases often leaves us with a fragmented foundation to maintain.
We mitigate this with abstraction, separating our domain from the foundation with libraries and wrappers.

Yet, by using many differing interleaved abstractions, we pay a cost..

+ By serializing into various formats, we expose ourselves to
    - injection on the server side or client side
    - loss of formatting or meaning
    - data translation surface area for defects
+ By communicating over various protocols to other processes
    - we give our applications and data more surface area to compromise
    - we have transmission security and session security
        * either may not be present in some protocols
        * nearly all implementations are inconsistent

## How we got here

With [DRY][], ["The best code is no code"][no code], and other morals driving highly productive developers, it is no wonder that we use other projects as our foundations.
Most companies employ developers to fulfill the needs of their domain.
Subsequently, most developers create what is useful to their domain, much of which cannot be shared to the public for technical or practical reasons.
Internally, developers aim to share code as much as possible, as this typically reduces work.

This post will focus on how we work with data.

Most [functional][] [requirements][] or [domain specific][] solutions are:

+ Reactions to data
+ Calculations on data
+ Emitting new data
+ Transforming data internally
+ Formatting
    - external input
    - internal data for external output

Whereas [non-functional][] [technical][] requirements ("how the system operates") involves:

+ Access, storage, removal of data
+ Iteration of data
+ Ordering of when domain specific code executes
+ Piping data between domain specific code executions
+ Scheduling domain specific code execution
+ Constructs to provide specific guarantees given conditions are followed
    - Locking
    - Atomic update
    - Atomic broadcast
    - Consensus
    - etc.

Often technical solutions are inherently reciprocal--both internally and publicly--and may advance to be foundational, such as the [Cassandra][] project by [Facebook][].

[DRY]: https://en.wikipedia.org/wiki/Don%27t_repeat_yourself
[no code]: http://blog.codinghorror.com/the-best-code-is-no-code-at-all/
[requirements]: https://en.wikipedia.org/wiki/Non-functional_requirement
[functional]: http://reqtest.com/requirements-blog/functional-vs-non-functional-requirements/
[non-functional]: https://en.wikipedia.org/wiki/Non-functional_requirement
[technical]: http://agilemodeling.com/artifacts/technicalRequirement.htm
[domain specific]: https://en.wikipedia.org/wiki/Domain-specific_modeling

### Technical Solution design

Issue witnessed is brought up. Initial solution is developed.

Over time, other issues come up, are resolved.

Duplication happens over course of time, or is foreseen to take place.

A pattern emerges.

A decision is made as a design.

First round refactoring takes place, a platform emerges.

Feedback witnessed, generalization takes place, platform improves.

Design revised.

Subsequent refactoring takes place, product adopts pattern and platform.

Platform promoted to foundation requirement.


### Legacy and risk

Technical solutions will acquire negative attributes in their lifetime.

A lifetime starts at conception, lives while being maintained, and dies when maintenance no longer occurs or is abandoned.

Maintenance includes enhancements and fixes.

Practically no software is immune to defects, especially defects in the design.

Development of a design results in features, which provides functionality desired by the product user.

Defects in the design are discovered during development of features or after initial development.

Enhancements add features after initial development.

Enhancements sometimes results in changing existing development, I'll call this *connecting* the feature, which includes code to *glue* the feature with existing features.

Enhancements themselves may contain defects in design and development.

The development to connect the feature may have defects.

Thus with enhancements, there is a risk of defects.

Features may be removed after initial development.

Removal of a feature is called deprecation.

Deprecation may be the result of obsolescense, meaning another feature with similar qualities may be available, requiring a change to the user's use of the product.

Deprecation may be the result of discontinuing a feature that is not reconsilable with other features of the product.
In other words, features may contradict or be inconsistent with other features, and the least impacting feature may be deprecated.

Deprecating a feature, or *disconnecting* the feature, may result in defects.

Thus deprecation carries a risk of defects.

A defect may refer to functional problems or technical problems, such as performance.

Defects often occur when a developer

* Is not aware of internal constraints, and violates those constraints
* Is not aware of external constraints, and violates those constraints
* Has a fundamental misunderstanding of the context or design
* Creates trivial logic errors
* Uses previously unreached logic, resulting in undefined behavior
* Is not aware of side effects caused by using other development, which affects other development after execution, these may be considered violating implicit constraints
* Is not aware of side effects caused by own development, which affects development after execution
* Creates a non-optimal or less optimal solution, resulting in using more resources including computation time
* Redundantly performs an operation which results in additional resource costs and side effects
* Does not take into account concurrent execution that result in side effects witnessed during execution, resulting in undefined behavior


A fix resolves a defect.

Fixes involve correcting existing development.

Fixes may alter the design of the product.

Fixes may be *quick and dirty*, meaning that it corrects certain logic but may have a technical penalty.

Technical penalties may contain technical defects and *legacy* penalties.

Fixes may result in additional defects, some defects may be expected such as technical defects.

Therefore, fixes carry a risk of defects.

Legacy is when logic or glue is less comprehensible, harder to reason about, and is less predictable.
Legacy may also apply to outdated documentation and incorrect interpretations of existing code.
Legacy may also apply to features and fixes making certain operations redundant.

Legacy occurs over time as quick fixes or quick enhancements negatively impact the codebase of the product.

Legacy affects developers and internal users negatively and may result in improper or incorrect design and development, subsequently resulting in defects.

Thus, the full lifetime of a product carries risk of defects.

Risk may be approximated and taken into account over the lifetime of the product.

Some products may not treat legacy proactively, prolonging legacy and disrupt accounting risk.

Analogously, resolving legacy is meta-maintainance, reducing risk of active maintenance.

However, like fixes and enhancements, resolving legacy also may result in risk of defects.

### Using technical solutions



Technical solutions often involve reuse of patterns and code, idioms that developers create along the way.

When developers create large scale products, they optimize by removing the redundancies.
However, often this results in some form of ceremony, boilerplate, a template of code that must be followed at every step.
Another term is embellishment.
http://thinkrelevance.com/blog/2008/04/23/refactoring-from-ceremony-to-essence
http://blog.codinghorror.com/code-smells/


We constantly seek reusability in what we develop, and in that

After preliminary development, we may notice patterns in our code.

In the process of designing solutions, developers discover patterns and in turn create tools to use those patterns effectively and easily.

...


Usage


### Technical product lifetime

While designing a fast and flexible solution for messaging, [Facebook][] created [Cassandra][].
However, Facebook found it to not solve their needs and [Apache][] soon adopted Cassandra.
Apache maintains and enhances many other industry-used software products, some of which is abandoned by their original creator.

Twitter's [Pants][], a build system, is not only used by the social media company Twitter, but even an insurance company like [Clover][].
Although tools are influenced by the designer's needs, they are ultimately reusable.
While some tools are released free, others are financed or supported with a variety of models.

+ Open or closed source by license: [Oracle][]
+ Open source with paid support: [NGINX][]
+ Open source with enterprise extensions [MySQL][]
+ Business tools maintainer, like [redislabs][] supporting the creator of [redis][]
+ Crowdsourced support, by small fixes and enhancements coming from many volunteers

[Facebook]: https://code.facebook.com/projects/
[cassandra]: https://en.wikipedia.org/wiki/Apache_Cassandra
[apache]: https://en.wikipedia.org/wiki/Apache_Software_Foundation
[pants]: http://pantsbuild.github.io
[clover]: http://www.cloverhealth.com
[mysql]: https://en.wikipedia.org/wiki/MySQL
[oracle]: https://en.wikipedia.org/wiki/Oracle_Database
[nginx]: https://www.nginx.com/services/
[redislabs]: https://redislabs.com/press-releases/redis-creator-salvatore-sanfilippo-antirez-joins-redis-labs#.Vbv9M0KJn8s
[redis]: http://redis.io

## Qualities of a solution

### Quality for Developer and Users

Users are also developers.

Users should be able to look inside and understand how/why.

Users should expect for platform to simplify and gain efficiency.

Users should expect gradual changes, compatibility breakages across major versions, but still fix-support for released versions.

### Generalizing a foundation

### Long term plan
Prevent feature bloat.

Prevent legacy creep.

### Consistency
+ Consistent communication protocol
+ Consistent communication format
+ Consistent security


## Another way to think of it
A Service Micro Kernel!
