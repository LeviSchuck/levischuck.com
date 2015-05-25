---
layout: post
title: This Game Engine is Futuristic!
description: Supporting mechanisms of Asynchronous calls and state machines.
category: Notes
tags: [Java, FSM, concurrency, asynchronous, data]
tagline: Yes, yes, I'm not deaf!
comments: true
---

Today I was thinking about what I had written in the last post, and what mechanisms would best support an asynchronous calling mechanism for data.

Then a conversation hit me that I had with a friend that has the same first name, the word __Future__ stood out to me. 

So I searched for *Future Java*, and found a [Future Interface][Java Future]. 

```java
public interface Future<V>
```

This is great, to me, because we have a standard that a lot of developers use.
<!--more-->

## Concept of Future<>;

You say at some point that an object of type `Future<T>` will get something you need. 
Perhaps you need to get the default quantity of fish at a fishing spot, but meanwhile you know and can process the default quantity of ores at a quarry. 

Therefore, you make an object of type `Future<int>` and set it to an object which will work asynchronously with a request for the fish quantity. 
You then process the quarry, and come back.
You then call `.get()` on the fish quantity, because you are ready to process it. 
If it hasn't *gotten* the fish quantity yet, the function call will __block__ and execution will sleep until the value is retrieved. 
However, if it did get the value while you were processing the quarry, then you can call `.get()` and there's no delay in the execution.

The point here is that you make an object that will promise to get you a value, some time in the __Future__. If it is unable to, you'll get an exception.

## Finite State Machines.

*No, I don't mean the Flying Spaghetti Monster.*

If you don't know what an [FSM][] is, I suggest you read up on it.

One thing that Java has, that doesn't seem to be in any other language that I'm aware of *or at least used*, is that each enum constant can act like it implements a class statically, but be treated polymorphic when it is an instance.

So, an enum instance can have functions!
Interestingly, [there's more][java enum] to Java enum than I previously thought. 



__The following is possible__

### Transition.java

```java
/**
 * To transition to another state, return a Transition instance.
 * The tokenType should be non-null if a token needs to be emitted, and
 * nextState should be the name of the class that needs to be transitioned to.
 */
class Transition {
	private final State nextState;
	private final TokenType tokenType;

	// Constructor
	Transition(State nextState, TokenType tokenType) {
		this.nextState = nextState;
		this.tokenType = tokenType;
	}

	// Access Methods
	public State getNextState() {
		return nextState;
	}
	public TokenType getTokenType() {
		return tokenType;
	}
}
```

### TokenType.java
```java
// These are tokens that are emitted.
public enum TokenType {
	GOT_ONE, GOT_TWO, END;
}
```

### State.java
```java
enum State {
	Q1 {
		public Transition nextTransition(int c) {
			State nextState = Q1;
			TokenType tokenType = null;
			if(c == 1) {
				nextState = Q2;
				tokenType = GOT_ONE;
			}else if(c == 0){
				nextState = Q1;
			}else{
				tokenType = END;
			}
			return new Transition(nextState, tokenType);
		}
	},
	Q2 {
		public Transition nextTransition(int c) {
			State nextState = Q2;
			TokenType tokenType = null;
			if(c == 1) {
				//No change in state
				tokenType = GOT_ONE;
			}else if(c == 2){
				tokenType = GOT_TWO
			}else{
				nextState = Q1;
			}
			return new Transition(nextState, tokenType);
		}
	}
	;// End of list of states

	/*
	 * Determines which state to transition to on the given input character
	 * and determines which token type to emit.
	 * This method must be implemented by all of the enum constants.
	 */
	public abstract Transition nextTransition(int c);
	//When you specify an abstract function here, you are saying that all
	//enum entries must implement it.
}
```

This state machine recognizes `1`, `0`, and `2` effectively.

It emits tokenTypes in the transition, or at least has them available for whatever pumps the machine.

Most other languages have an enum type, which at compile time reduces to an integer, but you have to make a huge `switch` case block with a lot of duplication to handle state machines. Java however lets you do something like 

```java
State state = State.Q1;
int input_int = 0;
while(...){
	//Something sets input_int
	Transition t = state.nextTransition(input_int);
	//Record t.getTokenType if not null
	if(t.getTokenType() == TokenType.END)
		break;
	state = t.getNextState();
}
```
	
It may not seem immediately useful, but state machines are useful within their own scope, states like

* In Game
* Main Menu
* Settings Menu
* Pause

can be helpful, and instead of an int, you have some sort of event.

This kind of stuff will be useful within a *Game Interface* process, and for simple AI as well.

You can even use state machines that eat emissions from other state machines to do some pretty cool things. 

## Java YAML

Looks like the best one that is currently maintained is [SnakeYAML][Java YAML].

Again, YAML is the human-readable data structure I plan to use, along side [Google Protobuf][] which is binary.

[Java Future]: http://docs.oracle.com/javase/1.5.0/docs/api/java/util/concurrent/Future.html "Java Concurrency Future Interface"
[Java YAML]: http://code.google.com/p/snakeyaml/wiki/Comparison
[FSM]: http://en.wikipedia.org/wiki/Finite-state_machine
[Google protobuf]: https://developers.google.com/protocol-buffers/docs/overview
[Java enum]: http://docs.oracle.com/javase/tutorial/java/javaOO/enum.html