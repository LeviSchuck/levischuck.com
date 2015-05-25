---
layout: post
title: Getting POCO setup
description: Getting POCO set up for compiling on platforms
category: Setup
tags: [POCO, libraries, C++, environment, windows, linux, mac, library]
tagline: Do you know who ate all the donuts?
comments: true
---
So what is [poco][]?

> Modern, powerful open source C++ class libraries and frameworks for building network- and internet-based applications that run on desktop, server and embedded systems.

Essentially, it will be my chosen framework to get things running efficiently and quickly.
<!--more-->

## Installing
### Linux

To install on debian-like linux distributions, `sudo apt-get install libpoco-dev` will do the trick.

Otherwise, go and [download poco][poco download] for "Linux, OS X, etc.", 
Extract, navigate to the directory in the terminal, run `./configure` then `make`, it will start compiling and will take a good while.

Once you have input to the terminal do `sudo make install` and let it install the header files and libraries to their respective directories.

### Windows

This is a little more complicated.

**I will be assuming that you use Visual Studio 2010.** *(not express)*

First of all, [download poco][poco download], and extract the zip for windows.

Inside, you will see multiple files, go ahead and run `build_vs100.cmd`

![](/images/win-poco-cmd.png)

This will take a while, and will likely make your computer hot, so don't be playing games or watching video as the compilation process is running.

Once this is done, the `lib` and `bin` folder will have the `.lib`, `.dll`, and other assisting files needed for this. 

Go into the `lib` folder and select the Poco specific files. The rest are for testing purposes which you do not need. Copy the selected files.

![](/images/win-poco-lib.png)

Now, locate the Visual Studio 2010 installation directory, go to `VC`, then `lib` and paste the files inside. You will be in the right folder if you see `.obj` files in there.

![](/images/win-poco-vc-lib.png)

You have now copied the compile-time requirements for the linker.

Now we need to set up the header files in the include directory.

Go up a directory, to the `VC` directory, and go to the `include` directory. Create a new folder called `Poco`, not POCO, but just Poco.
Enter the directory and it should be empty.

Have another window open to copy from, you will need to copy the header files next.

Back inside the poco directory, we will need to look in the following folders

* Foundation
* Net
* Util
* XML

Visit first the `Foundation` folder and go to the `include` directory.
Inside there is a `Poco` directory, go into that.

Next, select all the files, ~300 will be selected. Copy them.

![](/images/win-poco-headers.png)

Now paste these into the `VC\include` directory.

Next, `Net`, `Util`, and `XML` will follow this pattern.
Visit the folder, go to `include`, then `Poco` and you should see just one or more folders *(`XML` has multiple)*.

![](/images/win-poco-include.png)

Your `VC\include\Poco` folder should look like this:

![](/images/win-poco-vc.png)

And you're done!

### Mac
**I assume you have Xcode 4.* installed from the [Mac Appstore][xcode appstore]**

We could build from the sources directly, or we could be really easy and go with something that could help us later. We're going to go with [homebrew][].

![](/images/homebrew.png)

Go ahead and download it according to the instructions at the bottom.

Run `brew update` then `brew install poco`

It will take a while, but it should have an output similar to this.
*I have an outdated Xcode install, but it does not matter.*

![](/images/brew-install-poco.png)



[poco]: http://pocoproject.org
[poco download]: http://pocoproject.org/download/index.html
[homebrew]: http://mxcl.github.com/homebrew/
[xcode appstore]: http://itunes.apple.com/us/app/xcode/id497799835?mt=12