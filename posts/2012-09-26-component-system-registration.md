---
layout: post
title: Component System Registration
description: Thoughts about component systems, events, and registration
category: Notes
tags: [engine, thoughts, components]
tagline: I hate subways.  
comments: true
---


## Components And Properties

So, Properties are essentially strings that define what kinds of 
behavior might be associated with an entity.

A property also has a set of attributes and types that should be
enforced to exist with that property. For example, an RTS unit
may have certain status attributes, health, etc.

This will likely be defined in YAML for game-specific components.
However, the engine will not be responsible for loading the YAML
directly. The Game portion will specify that over the network.
Engine components will define the attributes they require in the
registration process.

Additionally. I am supposing that Components may listen for one
or more property. Thus combinations of properties may be possible.
Why do this? It would be easier to have a combo property component
than have components coordinate with each other for behavior.
<!--more-->


## Properties and their components
Properties for an entity are conceptually just a string in a list.
However, I plan to extend Properties such that they keep personal 
reference to the components that are listening, and the attributes
that this property is interested in. 

Messaging for attribute changes will follow this sort of pattern.

### Entity Modification

1. Entity Gets told to change Attribute
2. Entity asks the attribute *Is anyone else looking at you?*
    If the current Entity is the only observer within the engine,
    it will change the value and do the notification on the next
    step. Else, it will create a new Attribute from the factory
    and put it in a queue that will be processed later in one
    discrete execution.
3. Entity Puts the entity ID into a global queue which will be
    sent over the network loop-back to the game code, if the id
    is positive. *The purpose behind negative IDs will be discussed
    later.*
4. Entity traverses all Properties attached to the entity and
    inserts it's IDs into their queues.

These things happen within `process_engine_components()` and 
`process_queues_changed_by_game_code()` functions as used in the 
abstract code segment below.

### Game Loop

In the Game loop, we have a section that can be abstracted to..

```cpp
while(game should run){
    //The following code will cause entities attributes to change
    process_engine_components();
    //We pick up stuff from the game code asynchronously and finally pump them
    //out in the next function.
    process_queues_changed_by_game_code();
    //Entity changing stops
    render();
    //Process for the next frame
    pump_attribute_changes();
    commit_changes_over_network(); 
    //Game code should start doing stuff by this point asynchronously
    process_attribute_queues_from_properties_for_components();
}
```

### Property Dispatching to Components

As noted in the last step that Entities execute, properties
will have queues that will need to be processed so that 
engine components can be aware of external changes, *respective
to the component in question.*

Each property will contain references to instances of components
which implement / extend an interface. 

Such an interface includes functions like

+ `onEntityCreated(entity_id)` 
    The entire entity should be queried at this point,
+ `onAttributeChanged(entity_id)` 
    It would be redundant to include the individual attribute,
    since attributes are likely changed more than one at a time.
    It is up to the component to select the relevant attributes.
+ `onPropertyAdded(entity_id)`
    The entity previously existed, but **this** property has been
    dynamically added
+ `onPropertyRemoved(entity_id)`
    The entity will still exist, but **this** property has been removed
+ `onProcess()`

*This will probably apply to the game side as well.*

The steps a Property would take to notify components would be..

### Preconditions for Property event pumping:

For Property Instances

+ There exists a set of references to components.
+ There exists a set of Attributes which describe what Entities are 
    required to have.

Components implement their queues as sets, since calls may be made
multiple times for the same entity.

Entities have already traversed their properties, signaling to the 
properties that an entity has changed. The Attribute specific is not
given, as it is not important since it would be better off to consider
the entity as a whole.

*Notice: I've been using the term queue by meaning something that is to
be processed.* Not necessarily something **that is destructively consumed**.

Property pumping will follow an execution similar to

```actionscript
For each Property that is known
    //Each property has it's own list. Assume the following is a template
    //for each list.
    If Property has a queue / list to process then
        For each Component this Property references
            For each entity_id in the queue to process (non destructively)
                //This example instance is for attribute change.
                Component.onAttributeChange(entity_id)
            end For
        end For
    end If
    Clear Lists / Queues Here
end For
```

This does **not** actually process the changes. These functions
merely copy data into their *private queues* which will later be
processed with the `onProcess()`

## Component Registration

Properties will come into existence once something states that
they exist for an arbitrary purpose. For example, Properties can
exist when an entity states it has that property, even though no
components are actually hooked up for it.

Components can register that they listen to an entity, even though
no entities currently are using it.

This makes for a dynamic system in which components can be added
or removed for debug purposes, and also once a property exists, 
it will persist as a singleton until otherwise cleaned up.

Components will use static objects, or areas, I haven't determined
which yet to use. Areas mean that it will have to be called
like a tree, while static means it will be registered at some point
likely using singletons, before main is even started.

However, static has no defined order. Areas seem to be better if 
I plan to have components and such defined within game-modules 
which can be optionally statically linked in.

It seems that argument is winning.

The registration, regardless, will happen before the main game loop starts. 

I believe it was specified in a previous post that a component have the
interface functions initialize, deinitialize, and so forth. These functions
will tell the properties to register this interface instance, which will
likely be a singleton. 

I anticipate that the means to register would be rather standard to make
it easier and more dependable, same with unregistering. 
Functions that are similar to the following might be used.

```cpp
void registerComponent(Component *, const vector<String> Properties)
void unregisterComponent(Component *, const vector<String> Properties)
```

----

Also, Components ideally should be able to provide a means to serialize
if they do anything extra, since attribute manipulation can be costly,
and unnecessary when the data is not shared.