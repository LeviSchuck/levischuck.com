---
layout: post
title: Assumption Failure
date: 2013-06-02 19:27
comments: true
categories: Social-Network insane
---

You know what is somewhat demotivating? Having a month's work and more than a month in thought being put down the drain. What could do that? An assumption, that the bottleneck you planned for turned out to be much more of a bottleneck than you thought.

<!--more-->

I am working on a social network which I am trying to make real time, fast, light on the servers, and unique. My biggest priority was trying to be light on the servers, in memory and CPU. So I elected to go with C++11 for the main part and have node.js be a simple middleman proxy over ZeroMQ with my own simple protocol for an API server. That way I would have hope that node.js would not suffer any memory leaks and that it won't be overloaded within its single process design.
I managed to get the foundation working for my C++11 web engine, workers and all after about a month. Then came time to do the last piece, node.js. I was able to get my hello world demo route replying in about 5 lines, Yay! But not so fast, I needed to wrap it up to make a simple and hopefully consistent API. I couldn't get the dealer socket to work, so I tried a pool of request sockets. In the end, where my C++ engine could reply at 13000/s, node was only able to do 500 with one socket, and 750 for 10 sockets.

You can imagine the __let down__.

I heard that Node.js's ZeroMQ was slow.. but I didn't think it was _that_ slow.

Although I learned a lot, and I am extremely confident there are no memory leaks, I'm going to try to start over again with just node.js or Haskell. Although I don't know Haskell very well, I think it might be the better solution to go with.

You can find my sources released [on github](https://github.com/Cordite-Studios/cpp-web-engine).

