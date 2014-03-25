---
title: Web Service Philosophies
date: 2014-03-24
---
<!--- \[\d+/\d+/\d+ \d+:\d+:\d+ P?A?M\] .*levi[^:]+: -->
<!---
# authors:
# - name: Levi Schuck
#   email: me@levischuck.com
#   md5: 1447312a43014ccee8c6b7b357afa294
# - name: Philip Vieira
#   email: philip@vallin.se
#   md5: e43f261e9ff9d2bf1ddca57185ce0f78
-->

[Levi]: http://gravatar.com/avatar/1447312a43014ccee8c6b7b357afa294
[Philip]: http://gravatar.com/avatar/e43f261e9ff9d2bf1ddca57185ce0f78

|  Author     | Message                                       |
|:-----------:|:----------------------------------------------|
| ![Levi][]   | I am getting somewhat philosophical about web APIs right now. |
| ![Philip][] | Care to share?|
| ![Levi][]   | Well, I am seeing JSON as less of a viable option
| ![Philip][] | Because I start to develop some sense of philosophy myself. |
| ![Levi][]   | It is not hypermedia-aware, and many things like Siren, HAL, etc. attempt to make it aware, but in the end it is just mimicking HTML. |
|             | https://github.com/kevinswiber/siren Look at the example there. |
|             | The benefit to using JSON is that it is kill-over-easy to parse. |
|             | But it only supports objects, arrays, floating numbers, and strings.|
| ![Philip][] | Which is a strength in many cases. |
|             | I wish it supported dates though.|
|             | But you're pretty much fine with unix time.|
| ![Levi][]   |  stop guessing my writing
| ![Philip][] | And it's supported across a lot of platforms. |
| ![Levi][]   |  You end up with problems like Github where you don't have a native way to represent a date.|
|             | https://api.github.com/rate_limit |
|             | Am I to guess that is a unix time stamp? This requires programmer-knowledge of the API, whereas it should be intuitive and human readable as well as machine parseable. |
|             | A problem with unix time stamps is that not everyone agrees on which second represents which date due to leap seconds, days, and so on. |
|             | We have standards like ISO 8601 for dates, we should use them. |
|             | we have RFC's like 2119 which specify the language for things (MAY, MUST, SHOULD NOT, etc.) yet we don't use it in our designs. |
|             | Some people are like "XML all the things!", especially in java and microsoft land. |
| ![Philip][] | But.. |
| ![Levi][]   |  But those are not hypermedia aware either. They just have a benefit of being a-little more self documenting and locale friendly.
|             | `<message en:kind="Sending a file" es:kind="Taco taco" href="http://...." />` |
| ![Philip][] | There is a human aspect to this as well. There is something compelling in the simplicity of JSON |.
| ![Levi][]   |  JSON is simple, but only after the fact that the developer has to read the documentation, figure out what to pick and choose, and hard code URLs into their codebase with a "/v3/" prepending every request.
|             | I think that JSON is a fine format to put data in when sending a request, but not as a response. |
|             | http://tools.ietf.org/html/rfc6570 I find that URI templates is a very cool concept, but I'm not confident that they are the way to go. |
|             | I recall you looking into using something like that. |
|             | I think that instead of the user building the URL with such, it would be better to have a request like
|             | `/users?search=cordite`
|             | which does a 302 or a 301 to `/users/cordite`
|             | instead of `/users/{user_name}`
|             | https://api.github.com/ They extensively go for uri-templates.
|             | Consider |
|             | `"repository_search_url": "https://api.github.com/search/repositories?q={query}{&page,per_page,sort,order}",` |
|             | I see some values like page, per_page, sorting, order as secondary parameters. |
|             | The problem is that "page", "per_page", "sort", "order" don't mean anything to a machine. |
|             | It requires a programmer to look at the documentation to see what options are available. For sort I have no clue here what values I can sort by, order? I could *guess* desc or asc, or it might be descending or ascending, up or down, or even "random" |
|             | You don't need documentation to use Facebook, google, or twitter. Your options are expressed in front of you. |
|             | Also, I think the whole CRUD-API thing is disgusting these days. |
|             | The service should still be located on the service's server, not the client. The external representation should not be constrained to be the same as the internal representation. |
|             | One of the reasons Facebook backed off on having a smart client like what users of ember build, is that these smart clients are not only fat, but are harder to test, debug, control, and do fundamental iteration with like A/B testing. |
|             | Another reason is simply that they can measure what happens on their servers, but on the client side? They don't have profiling on the masses to see that 5% are on phones that lack enough memory to act within desirable time constraints (or in my case crash when out of mem). Yet they can at least say that the browser should rerender and show the new content they-sent within 50-100ms. |
|             | Yes, data minimization is important--so use gzip on your servers. Github purposefully makes their API so that it is actually white-spaced to be more human friendly. How much do they pay in terms of bytes? usually 4-10, thanks to gzip. |
|             | I think it is time for you to attack, contemplate, or support what I said :P |
| ![Philip][] | I am :D |
|             |Give me a minute |
|             |I'm still at work |
| ![Philip][] | back |
| ![Levi][]   | Alright. |
| ![Philip][] | I have been thinking about the URI template and found myself mostly using it in situations where I want to pass several filters to a collection. |
|             | As for search |
|             | Search is a different story, I have come to treat a search as a resource in and out of itself. And that you POST the search containing parameters. |
|             | In terms of user experience, most application I see stick to *one* search box to retrieve may different resources. |
|             | Which certainly align with treating it as it's own resource. |
|             | Then the URI templates is not even close to being enough documentation to show the depth of how you can treat the resource. |
| ![Levi][]   | Class will be an hour long, feel free to tell my your experience and philosophy in the mean time. |
| ![Philip][] | I'm interested in how you go about documenting resources expecting payloads. |
| ![Levi][]   | I am back. |
|             | Payloads to the client or payloads to the server? |
| ![Philip][] | I was talking about payloads to the server |
| ![Levi][]   | Yeah, www-form-encoded or whatever, JSON, XML, EDN, BSON, MsgPack, Protobuf, I don't care. |
|             |But www-form-encoded ought to be supported :P |
| ![Philip][] | But I mean, in a hypermedia driven API |
|             |You simply don't? |
| ![Levi][]   | not as much |
| ![Philip][] | Hypermedia only applies for relative gets? |
| ![Levi][]   | no, hypermedia has to do with `<form>`'s as well |
| ![Philip][] | oh |
| ![Levi][]   | in a hypermedia driven API, all interactions should be able to be defined at least by www-form-urlencoded, which is really just a key-value sequence. |
| ![Philip][] | which is ludicrous. |
|             | in some cases |
| ![Levi][]   | It cuts down on things, sure |
|             | Go ahead, talk to me about why it is ludicrous |
| ![Philip][] | I'm mostly concerned by the web server layer |
|             | Varnish + Apache/Nginx |
| ![Levi][]   | And how does a strictly key-value sequence cause concern here compared to the client->server payload being json? |
| ![Philip][] | url encoded as part of the url or of the payload? |
| ![Levi][]   | Any mutable activity should never be in a GET, so I would assume it would be in the payload if it is something significant |
| ![Levi][]   | If you send more than 63K in the URL, I shake my head at you. |
| ![Philip][] | Yeah, that's what I meant. |
|             | That's what's ludicrous, if it's in the payload I don't really care what the format is. |
| ![Levi][]   | I wouldn't suggest trying to encode bson in the url ;) |
| ![Philip][] | I did try once :D |
| ![Levi][]   | And how did Ruby deal with that? |
| ![Philip][] | It was fine when I pulled it through my mongo driver |
| ![Levi][]   | I see. |
| ![Philip][] | It was more of an experiment |
|             | Never actually used it. |
|             | When it comes to rails you can submit XML, YAML, JSON & www-form-urlencoded by default. |
|             | multipart is totally legit as well |
| ![Levi][]   | In case you are interested. |
| ![Levi][]   | http://levischuck.com/posts/2012-10-29-bson-vs-yaml-vs-protobuf.html |
| ![Philip][] | What I like with BSON is the ids and the dates. |
| ![Levi][]   | I did some measurements at the C/C++ and socket level. |
| ![Levi][]   | Yeah, native UUID and date time support is a blessing. |
| ![Philip][] | The problem is the use with web clients. |
| ![Levi][]   | I have to go, please continue on with your philosophy (this time) |
| ![Philip][] | It's like late here, I won't be able to go on for too long. |
|             | When designing an API, currently, there are three things that haunt me. Platform support of the payload content type (full web compatibility is a minimum). Payload size and the performance of compiling the payload. Usability in terms of the implementation's language and object model. |
|             | And that's only looking at the payload itself. |
|             | Then there's another thing, REST, while it make for a very readable and straight forward API have not much to say about how an event driven application should work. |
|             | If you want to represent changes, deletions and additions purely with data. How would you go about it? |
|             | Would you consider all these things resources? Is the event a resource? The more things you consider a first class citizen resources in REST the more overhead you'll get. |
|             | I'm getting more and more interested in how to solve the problem of how to build a resource where you have multiple streams of information (Socket or Request) |
|             | My philosophy has started to form that you need to build your API around the two concepts of both requesting and being served the current state of a resource with as little overhead as possible. |
| ![Levi][]   | I am actually dealing a lot in my writing about event driven applications. I have yet to finish my 2nd post (out of 4 at minimum). I believe I have a solution for it. |
|             | I do not have as much of an attachment to the idea of a fixed resource. |
| ![Philip][] | I'm starting to loose the idea that a fixed resource is the way to go. |
| ![Levi][]   | I like the idea of CQRS+Event Sourcing, where your read layer is completely distinct from your write layer, and with events merely contributing in an append-only fashion for representing the resource / entity state. |
|             | Check out CQRS A journey guide if you are interested in architecture |
| ![Philip][] | The book by microsoft? |
| ![Levi][]   | Yes. |
|             | It is full of azure-isms, but the concept is still there clearly. |
| ![Philip][] | Ah yeah |
| ![Levi][]   | My next article will be about web hooks and discoverable APIs |
|             | This of course defines service <-> service architecture, not client <-> service event architecture |
| ![Philip][] | yeah |
| ![Levi][]   | I suppose my interests lean towards the service to service layer and yours is with the client? |
| ![Philip][] | Yes, indeed. |
|             | Servers have immense computational power compared to a phone or a shitty laptop. |
|             | What interests me is how to solve the architecture to increase the performance of a heavily network bound event driven app. |
| ![Levi][]   | I know I saw something relevant to this, hold on. |
|             | http://www.realtime.co/ Ah, here it is. |
| ![Philip][] | Anyway, if one were to go with the HTTP REST model and have the concept of set resources. I've been quite intrigued by the idea of having user defined resource GUID's |
|             | To solve the problem of transience. |
|             | Clients can internally refer to a resource that is not yet mandated by the host. |
| ![Levi][]   | define transience in this context |
| ![Philip][] | if the client get to define what identifies the resource before the host is aware of it, you can allow for lazy synchronisation without disrupting the workflow in the client. |
| ![Levi][]   | Indeed. This is a preferable attribute. |
|             | Instead of a single UUID, I was thinking where each resource contains two, one which IS known by the host which represents the actor--the second which is created / suggested by the actor. |
| ![Philip][] | I agree |
|             | Then the performance is an implementation detail. |
|             | I'd like to discuss a "stream" resource at a later point |
| ![Levi][]   | Have you looked at the realtime.co thing yet? |
|             | Like Salesforce with Apex, they've created their own proprietary technology which I don't like. |
|             | It is still worth analyzing |
| ![Philip][] | Yeah, I'm a little sceptical |
|             | I guess I just need to spend some time reading |
| ![Levi][]   | http://levischuck.com/posts/2013-08-25-facebook-theories.html |
|             | Here's how I deciphered facebook's method, it's really just a one-way web socket that happens to be compatible with most browsers. |