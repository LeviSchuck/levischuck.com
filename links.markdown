---
title: Link Dump
---

Generally the links that I will dump here will
not be largely known. An example of what is well
known is [MySQL](http://www.mysql.com/).

# Databases

+ [RethinkDB](http://www.rethinkdb.com/) JSON oriented distributed database
+ [WhiteDB](http://whitedb.org/) A portable in-memory database with simple query support, graphing, and fast interprocess communication.
+ [RavenDB](http://ravendb.net/) 2nd Generation document .NET database
+ [ArangoDB](http://www.arangodb.org/) Flexible model database with an SQL-like language
+ [OrientDB](http://www.orientdb.org/) Lesser known Graph database in Java
+ [Datomic](http://www.datomic.com/) An immutable database with local queries
+ [InfluxDB](http://influxdb.org) Distributed time series, events, and metrics database in Go
+ [RocksDB](http://rocksdb.org) An embeddable persistant (with LevelDB) key-value store. Created by Facebook, released under the BSD license.
+ [Trousseau](https://github.com/oleiade/trousseau) Networked encrypted key-value database
+ [Deep DB White Paper](http://deep.is/knowledge/deepdb-white-paper/) How Deep DB uses append-only immutable data structures to provide a scalable database.
+ [FoundationDB](https://foundationdb.com) A distributed database that practically operates itself.
+ [KDB+](http://www.kxcommunity.com/features/) A high-performance database, now made free, with its own `q` querying / programming language.

# Server Technologies

+ [Docker](http://www.docker.io) "An open source project to pack, ship and run any application as a lightweight container"
+ [CoreOS](http://coreos.com) The container operating system/layer for Docker. Made in Go.
+ [dweet](http://dweet.io) A tweet-like event service for the internet of things.
+ [Algolia Search As A Service](http://www.algolia.com/features) A search-as-you-type service with fast response times.

# Languages of Interest

## Used
+ [Opa](http://opalang.org) Compile-To-Javascript functional language.
    Not really a *javascript framework* as the marketing buzzwords suggest.
    Really cool, but found to be __intensively slow__ in practice and __set off
    alarms__ for abuse at [dotcloud](https://www.dotcloud.com) for a
    [project](https://github.com/kloplop321/cactusdb)
    I made to keep track of plants. Runs on top of Node.
+ [Haskell](http://www.haskell.org/haskellwiki/Haskell) Most recently
     became a fond language for me. It is a __purely functional__ language.
     This requires a huge mind shift to use effectively.
     I feel this language has really helped me think as a software developer.
+ [ClojureScript](https://github.com/clojure/clojurescript) Compiles Clojure to JavaScript.
     It is really cool since I can do Go-like CSP, and with Facebook's react, make
     fast, dependable UIs that do not leak. Unfortunately, the JS that is emitted is
     quite fat.

## Interested but have not used
+ [Ceylon](http://ceylon-lang.org) Redhat's recently relased language for the web.
+ [Rust](http://www.rust-lang.org) Mozilla's concurrent and memory-safe language.
    Still going through constant breaking changes.

# Blogs I started following

+ [Jabberwocky](http://jabberwocky.eu/) Conference speaker, does ruby and Haskell
+ [Adit](http://adit.io) This guy writes entertaining and easy to read concept articles, like software-transactional-memory
+ [Antirez weblog](http://antirez.com) Guy behind Redis
+ [Maurício Gardini](http://mauriciogardini.com/) Made a tile-like window manager
+ [Datomic Blog](http://blog.datomic.com/) Datomic's blog and their progress
+ [The If Works](http://blog.jcoglan.com) Guy behind Faye, does Ruby and Node.js
+ [Aphyr](http://aphyr.com/) with his "Call me maybe" distributed services series
+ [Noel](http://noelwelsh.com) He does web services with Scala and has an emphasis on functional approaches for distributed systems
+ [Alvaro Videla](http://videlalvaro.github.io/) Erlang enthusist, author of RabbitMQ in Action
+ [Kirill Zubovksy](http://kirillzubovsky.com/) Founder of Scoutzie, focuses on designing and user experience
+ [Brian Hauer](http://tiamat.tsotech.com/) A blog filled mostly with rants, has a 4K monitor
+ [Steve Klabnik](http://blog.steveklabnik.com) A Rubyist who understands APIs
+ [Mike Amundsen](http://amundsen.com/blog/) Hypermedia enthusiast living life in lowercase

# Exciting front end projects

+ [Polymer](http://www.polymer-project.org/) a polyfill for upcoming component web standards
+ [AuraJS](http://aurajs.com/) RequireJS declarative components
+ [React](http://facebook.github.io/react/) Uses a virtual DOM to speed up the rendering process

# Random indivdual posts

+ [Fourier transform](http://nautil.us/blog/the-math-trick-behind-mp3s-jpegs-and-homer-simpsons-face) A bit on how encoding and lossy compression behind things can use some cool math.
+ [Feynman Technique](http://www.farnamstreetblog.com/2012/04/learn-anything-faster-with-the-feynman-technique/) Learn faster by teaching and simplifying the material in your own words!
+ [You are not a Software Engineer](http://www.chrisaitchison.com/2011/05/03/you-are-not-a-software-engineer/) You are a Software Gardener
+ [Edn Format](https://github.com/edn-format/edn) Like JSON, but represents sets natively.
+ [Raft](http://highscalability.com/blog/2013/8/7/raft-in-search-of-an-understandable-consensus-algorithm.html) An easily understandable consensus algorithm for distributed consistent data.
+ [Ebay and zero-downtime](http://www.ebaytechblog.com/2013/11/21/zero-downtime-instant-deployment-and-rollback/) How ebay manages to do instant deployment and rollback on nodes.
+ [StackOverflow's means to scale](http://nickcraver.com/blog/2013/11/22/what-it-takes-to-run-stack-overflow/) describes the intense load their services take on, how 384GB of RAM helps, and how important it is to make your code fast in the first place.
+ [Old SEO methods cause panic](http://www.theawl.com/2013/12/the-new-spammer-panic) If only this had changed much sooner..
+ [Real Time Systems](http://blog.flaper87.com/post/52e43f93d987d2cc17700b58/) This defines what real time really means
+ [AWS Tips](http://wblinks.com/notes/aws-tips-i-wish-id-known-before-i-started/) Using AWS definitely does seem to be a strong shift in thinking, this helps bridge the gap.
+ [UX Crash Course](http://thehipperelement.com/post/75476711614/ux-crash-course-31-fundamentals) Fundamentals in UX
+ [Little's Law in a Parallel Universe](http://blog.paralleluniverse.co/2014/02/04/littles-law/) By Parallel, he means running parallel in our universe. Interesting stuff when it comes to functional programming.
+ [Salted Password Hashing - Doing it Right](https://crackstation.net/hashing-security.htm) Some insights into better security design, which reminds me of
    [Security Through Obesity](http://ethanheilman.tumblr.com/post/28481738192/security-through-obesity).
+ [Saving your S3 Bill](http://www.appneta.com/blog/s3-list-get-bucket-default/) Apparently get_bucket and keys can jack up the costs, these guys reduced their costs by 90%.
+ [Swap Beta idea](http://blog.frontapp.com/dont-give-access-to-your-beta-for-free-swap-it/) How to get more active users by a bit of barter / trade for beta access.
+ [Micro Service Architecture](http://yobriefca.se/blog/2013/04/29/micro-service-architecture/) Defines what MSA is, which twitter uses and most large-scale systems like eBay as well.
+ [Load balancing in AWS](http://engineering.chartbeat.com/2014/02/12/part-2-lessons-learned-tuning-tcp-and-nginx-in-ec2/) They reduced their response time by 98.5% by moving from a homegrown solution to Amazon's ELB.
+ [Smartstack Service Discovery](http://nerds.airbnb.com/smartstack-service-discovery-cloud/) An inverted way of balancing services.
+ [Docker Base Images](http://phusion.github.io/baseimage-docker/) How you are probably doing docker wrong, and how you can do better.
+ [Regex Routing](http://nikic.github.io/2014/02/18/Fast-request-routing-using-regular-expressions.html) Rather than do a sequence of individual regexes, why not try to combine them? Invalid URLs can be determined much quicker.
+ [Flow Based Programming Concept](http://www.fastcolabs.com/3016289/how-an-arcane-coding-method-from-1970s-banking-software-could-save-the-sanity-of-web-develop) Although developed early on, this technique fell out of favor when it came to writing assembly for Von-Neumann machines. These days, it is significantly relevant, visual and audio effects artists use it, why not us?
+ [Users don't hate change](https://medium.com/design-startups/461772fbcac7) They hate what they percieve to not benefit them.
+ [C++ The Good Parts](http://www.infoq.com/presentations/c-plus-plus-pros) With plenty of mentions of Haskell, Jordan DeLong from Facebook talks about C++14 easing compile time functional concepts.
+ [Colors of the iOS App Store](http://blog.brandisty.com/brand-management-blog/colors-of-the-ios7-app-store/) Gives some insight on what the market seems to use for each category of service
+ [X86_64 Assembly](http://hackeradam17.com/2014/03/18/an-introduction-to-x86_64-assembly-language/) A quick introduction to help decypher what you see in a disassembler
+ [Algolia and client side security](http://blog.algolia.com/handle-security-realtime-search/) They show how they solve user distinction and limiting from the browser side communicating directly to Algolia's servers
+ [Coming back from the Brink](http://scott.a16z.com/2014/03/24/were-fd-its-over-coming-back-from-the-brink/) Scott Weiss's experience with events that might mean the ruin of a company (like Hotmail).
+ [Uniform Basis for Exchanging Representations (UBER)](http://g.mamund.com/uber) A specification for hypermedia in the future
+ [Prezi got pwned](http://engineering.prezi.com/blog/2014/03/24/prezi-got-pwned-a-tale-of-responsible-disclosure/) Prezi noticed that users could download `file:///` thanks to an email
+ [Double HMAC Verification](https://www.isecpartners.com/blog/2011/february/double-hmac-verification.aspx) Foils timing attacks while being simple to implement and keeps you cryptographically agile.

# Javascript Libraries

+ [JointJS](http://jointjs.com/api) A graphing library with canvas.
+ [Moment.js](http://momentjs.com) A javascript date library for parsing, validating, manipulating, and formatting dates.
+ [sigmajs](http://sigmajs.org/) Node graph drawing library
+ [Seneca](http://senecajs.org/) A Micro-Services toolkit for Node.js

---

More will come as they intrigue me.
