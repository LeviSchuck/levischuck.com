---
layout: post
title: C++11 constexpr and maybe UDL
date: 2012-10-20 13:25
comments: true
categories: Experiments
mathjax: true
---

I was looking further into the uses of constexpr, which can be used to resolve expressions at compile time. This may increase compile time, but it can be useful for not recalculating values at runtime. 

That's what `constexpr` is for, the compiler will select constant expressions which are fed to `constexpr` functions, and resolve them then and there. This can be especially useful for variantly long data, such as strings. For non-constant expressions, they'll be determined at runtime, so the function will be compiled and available with the same logic.

There are restrictions though, it practically has to be functional, but it also has to conform to C++ syntax. The result is that you can't have variables or if statements or loops inside them.

I first made an imperative version which I could debug with and step through. After I had something satisfactory, I made a constexpr version, which used recursion to get to the end of a string.

Though I'm bummed that user defined literals aren't as supported. GCC 4.7 and Clang 3.1 support it, but most linux distros are still on 4.6 _(Though I have tested using 4.7 and it works nicely, but it required getting a test dev install)_. Mac's _Apple Clang 4.1_ is a bit deceiving. They have different version numbers than actual clang. The rest of the version string says _based on LLVM 3.1svn_, which **only** means that it was grabbed between _3.0_ and _3.1_.

So, I can't use User Defined Literals _(ULDs)_ yet.

The source I tested with is as follows:

<!--more-->

```cpp
#include <iostream>
using namespace std;

unsigned long attrID2(const char * ls){
   const unsigned long golden = 0x9E3779B97f4A7C15;
    unsigned long result = 0;
    int offset = 0;
    for(int i=0; ls[i] != 0; i++,offset = (offset + 1) % sizeof(long)){
        const unsigned long p = ((long)ls[i] << (offset*8));
        const unsigned long p2 = ((long)1 << (i%(sizeof(long)*8))) & golden;
        result = ~0 & (result ^ p ^ p2);
    }
    return result;
}

unsigned long constexpr attrID3(const char * ls, long val = 0, unsigned int depth = 0, unsigned int offset = 0) {
    return *ls
        ?
        attrID3(ls+1,
                ~0 & (val ^ ((long)*ls << (offset*8)) ^ (((long)1 << (depth%(sizeof(long)*8))) & 0x9E3779B97f4A7C15)),
                depth+1,
                (offset+1) % sizeof(long))
        :
        val;
}

unsigned long constexpr operator "" _attr(char const * ls, std::size_t n){
    return attrID3(ls);
}


int main(int argc, const char * argv[])
{
    const char * test = "Some test text here";
    unsigned long a,b,c,d,e,f;
    char test2[] = "meh";
    a = attrID2(test);
    b = attrID3(test);
    c = attrID2(test2);
    d = attrID3(test2);
    test2[1] = 'a';
    e = attrID2(test2);
    f = attrID3(test2);
    
    cout << hex;
    cout << a << endl;
    cout << b << endl;
    cout << c << endl;
    cout << d << endl;
    cout << e << endl;
    cout << f << endl;
    return 0;
}
```

If you're curious as to what the `0x9E3779B97f4A7C15` is about, it's a magic constant being 

$\begin{equation}
	\frac{2^{64}}{\varphi}
	\end{equation}$

Which is to say, 2 to the power of 64 (the range of numbers of a 64-bit processor / data type) divided by the golden ratio. 

I've seen this elsewhere [used in encryption][nothing up my] and stuff, but a 32 bit constant instead. I don't know how useful it will be, other than I hope it will be sufficiently random to reduce collisions.

[nothing up my]: http://en.wikipedia.org/wiki/Nothing_up_my_sleeve_number

----

You can do `operator "" _attr("some string")` which will work, but that defies the whole anti-verbosity of being able to do `"some string"_attr` with suffix notation.

ULDs do not need to be `constexpr`, but I believe the same principles apply.

----

I'm also planning to drop plans to support windows using MSVC. I'll probably go with codeblocks using GCC 4.X in the future. 

Why does Microsoft have to be so behind the times?