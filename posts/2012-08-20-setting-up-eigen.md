---
layout: post
title: Setting up Eigen
description: "Eigen is a C++ template library for linear algebra: matrices, vectors, numerical solvers, and related algorithms."
category: Setup
tags: [C++, environment, windows, linux, mac, Math, library, Eigen]
tagline: I can see your MIT education really pays for itself. 
comments: true
---

What is [Eigen][]? 

> Eigen is a C++ template library for linear algebra: matrices, vectors, numerical solvers, and related algorithms.
It is completely contained within header files, there's no library linking, and it is standard C++98, it should compile on everything recent.
<!--more-->

### Why Eigen

Eigen is efficient, trusted for scientific applications, and doesn't have amateur limitations which custom implementation usually have.

## Installing
### Linux 

Follow the UNIX-like instructions.

Packages do not install it the same way, the debian package for example will install to eigen3/Eigen.

### Mac

Follow the UNIX-Like instructions.

Brew for some reason wants to compile extra stuff in FORTRAN, which doesn't make much sense for a header only library.

### Unix-Like

Execute the following. If there is a newer version of Eigen avaliable, 
```bash
cd Downloads
wget http://bitbucket.org/eigen/eigen/get/3.1.1.tar.gz
tar -xzvf 3.1.1.tar.gz
cd eigen-eigen-43d9075b23ef
sudo cp -r Eigen /usr/local/include/
```

### Windows
This one is actually simple, as there are no libraries.

Go and [download][Eigen Zip] the zip archive, unzip it and go into the folder, it should be named `eigen-eigen-` and then part of a seemingly random string of hexadecimal characters.
Open until you see several folders, including `Eigen`.

Open in another window your Visual Studio VC directory.
Mine for example is like `C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC`
Go to the include directory, and paste the Eigen folder in.

I believe you are done now. 

##Documentation

[Generated Documentation][Eigen Docs] holds several tutorials and advanced topics.

It provides many code examples with the outputs for what was executed.

Overall it seems like this is one of the best documented libraries I've seen so far.


[Eigen]: http://eigen.tuxfamily.org/index.php?title=Main_Page
[Eigen Zip]: http://bitbucket.org/eigen/eigen/get/3.1.1.zip
[Eigen Docs]: http://eigen.tuxfamily.org/dox/