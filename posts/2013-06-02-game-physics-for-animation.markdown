---
layout: post
title: Game Physics for Animation
date: 2013-06-02 19:55
comments: true
categories: physics animation
---

One of the things I did last semester in school was an original research project for our technical writing course. I was assigned to a team of other computer scientists that were in the class, and we had to come up with a research topic.

<!--more-->

At first we settled on how effective automatic feedback tools are.

I don't mean the manual ones that ask you when things crash

> An error has occurred, would you like to send a report to *COMPANY NAME HERE*?

We tried looking into academic resources.. and then tried google and google scholar and found **nothing**.

So, I had previous experience with [Bullet Physics](http://bulletphysics.org/wordpress/)
and thought that maybe we could do something with physics? 
Turned out there was plenty of research we could cite (a requirement), it was going to be an original experiment and it would be fun to do.
*The last formally comparative research on physics engines was in like 2005 or 2007 and its emphasis was on games.*
We decided to go for an emphasis on animation instead of games. What's realistic, what's acceptable, what's unacceptable?

This turned out to be a good decision because of the research, but ultimately, it was a bad decision because we didn't know how large the scope was before starting.

LibPAL, a Physics Abstraction Layer library, was outdated and can be essentially **considered dead**. We planned to use it, like the report from the last half-decade, but it wouldn't even compile right on my laptop. 

So, I worked on creating our own simulation abstraction layer, hooked it up with [Irrlicht](http://irrlicht.sourceforge.net/), and then began testing. The largest part.. was just getting the abstraction layer done. I had previously made a game engine which I consider a failure, because I hit my own walls and tried to break assumptions I unconsciously made. This helped me make a design that did exactly what I needed.

In the end, we were only able to use arbitrary boxes and spheres, but this was good enough for our paper and to have some conclusive results only a week before it was supposed to be finished.

Further, I wrote the entire [document](/images/physics.pdf) in LaTeX. I started learning and practicing with it once I got introduced to it when I had to write a mathematical proof for one of my computer science courses. It was really cool to never have to go and use Libre Office again. Now, the rest of the team did a lot of the accompanying research and actual writing, though I did the coding and writing of the conclusions.

You can find my code [on github](https://github.com/kloplop321/physics-tests).

I coded more for this English class than **any** other computer science course I've taken.