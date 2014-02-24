---
layout: post
title: Setting up Protobuf
description: Setting up Google Protobuf for development environments
category: Setup
tags: [Google Protobuf, libraries, C++, environment, windows, linux, mac, library]
tagline : "Dog, throw something bigger!"
comments: true
---

# Google Protobuf
What is [protobuf][]? I think that Google's own words will suit best here.

>Protocol buffers are Google's language-neutral, platform-neutral, 
>extensible mechanism for serializing structured data – think XML,
>but smaller, faster, and simpler. You define how you want your data
>to be structured once, then you can use special generated source code
>to easily write and read your structured data to and from a variety
>of data streams and using a variety of languages – Java, C++, or Python.

Basically, I will be using protobuf for binary data an messages 

[protobuf]: https://developers.google.com/protocol-buffers/
<!--more-->

# Installing

## Linux

To install on debian-like linux distributions, `sudo apt-get install libprotobuf-dev protobuf-compiler`.

Otherwise, go and [download protobuf][protobuf download unix].
Extract, navigate to the directory in the terminal, run `./configure` then `make`, it will start compiling.

Once you have input to the terminal do `sudo make install` and let it install the header files and libraries to their respective directories.

[protobuf download unix]: http://protobuf.googlecode.com/files/protobuf-2.4.1.tar.gz

## Mac

Assuming you already have installed [Xcode][] and [homebrew][], all you need to run is `brew update` then `brew install protobuf`.

Then, to make sure that protoc, the compiler, got installed correctly, do `which protoc` and you should get a result, if you got a blank result, then run `brew install protobuf-c`.
[Xcode]: https://developer.apple.com/xcode/
[homebrew]: http://mxcl.github.com/homebrew/
## Windows

Now we get to the lovely Microsoft environment. 

I assume you have Visual Studio 2010.

### Getting Started

Go and [download protobuf][protobuf download win], extract and go to the folder in explorer. Most of your work will be in the `vsprojects` directory.

The files are in Visual Studio 2008 format, so we'll let Visual Studio 2010 convert them for us. Go ahead and open the older version for `libprotobuf`.

![](/images/win-proto-vcproj.png)

![](/images/win-proto-up-wizard.png)

Go ahead through the process, you don't need to make a backup or read the conversion log at the end.
And don't worry about this either:

![](/images/win-proto-sec-warning.png)

### Building and actually installing the Libraries
Next, now that we should have our solution open after all the junk we had to do earlier, let's build it! We are by default in `Debug` mode, and we'll need both `Debug` and `Release` for later.

![](/images/win-proto-build-list.png)

![](/images/win-proto-build-output.png)

Now.. We're going to be putting debug and release libraries in the same folder in the end as we manually install them, and the files will be named the same, unfortunately. So we're going to rename the `.lib` that was just compiled.

Go to the Debug directory, and select it like so:

![](/images/win-proto-static-lib.png)

Open the Debug folder in the `vsprojects` directory.
Rename `libprotobuf.lib` to `libprotobuf-d.lib`, the "-d" being added in before the file extension to mark it, for us, as a debug library. Now, in another window, open up your Visual Studio 10 directory in `Program Files`, or `Program Files (x86)` or wherever you installed it. Open the `VC` directory and open the `lib` directory. Copy `libprotobuf-d.lib` into that directory, and accept the administrative action request.

Change the dropdown box at the top to `Release`, and then build `libprotobuf` again. This file has no need to be renamed, so just do the copy step.

Follow the same process and you should see your files looking like this:

![](/images/win-proto-copy-lib.png)

### Installing Header files
Google made it so that they didn't have file duplications in the archive you download, so they made a nice tool to extract them out.
Inside the `vsprojects` directory, there's a utility called `extract_includes.bat`, a script that will pull out and make an `include` directory for us that we need. Execute that and it will first ask you to confirm since it is an application/script/command that was downloaded from the internet.

![](/images/win-proto-headers-bat.png)

An include directory should be visible now.
Navigate inside and there will be a `google` folder. Copy that, and go to your `VC` directory back in your Visual Studio 2010 installation folder. Go to the `include` folder and paste in the google directory.

![](/images/win-proto-include.png)

### Protobuf Compiler
You may [download the compiler][protobuf compiler win] binary, or build it yourself if you prefer.
I'd suggest dropping the `protoc.exe` in your `\Windows\System32\` directory, or any path of your choosing which happens to be in the `PATH` environmental variable.


[protobuf download win]: http://protobuf.googlecode.com/files/protoc-2.4.1-win32.zip
[protobuf compiler win]: http://protobuf.googlecode.com/files/protoc-2.4.1-win32.zip
