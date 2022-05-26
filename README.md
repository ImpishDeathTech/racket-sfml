# racket-sfml 
### An Object Oriented, kebab case layer for racket-csfml

Why? Because I love sfml, I love racket, I love the way racket does OO and I like consistency
in my code. The csfml syntax does not fit with racket, and also drives me absolutely nuts in general.

It has a good number of helper functions defined because I find them necissery and useful in my own code. If you don't that's fine.
There are also a number of shorthands for certain functions, such as v2f-x for vector2f-x and so on.

If you know sfml/csfml, you shouldn't have too much trouble with this library, though I 
**will** be documenting it when I have time, because it does differ slightly in ways from the C++ version both in
order to conform with racket syntax and maximize ease of use. Many objects and structures and functions have been
renamed for this purpose as well.

As of now, this library is quite operational, but still in development and incomplete in some aspects. Tweak it as needed,
and let me know if you've improved anything or have any input at all, I'd **love** to take a look at what you have to offer,
or hear you out on what you think this library needs! ^,..,^

If you would like to use it with the `sf:` prefix, simply use `(require sfml/prefixed)`, otherwise, `(require sfml)` provides **almost** everything
you'll need. Untill I get everything fleshed out, just use csfml for what's not quite done yet.

Note that the objects will **not** work until you 
```racket
(send object make)
``` 
to create the pointer, and you should **always** free the memory with 
```racket
(send object kill)
```

if not, "set" methods will do absolutely nothing - save for "set-pointer" which can be used to manually set the object's pointer provided it is of the correct type indicated by the object - and "get" methods will generally return `#f` or an object with an empty pointer

you can check if the pointer is set with
```racket
; returns false or #<cpointer>
(send object pointer)
```
In order to load textures, fonts, sounds and other loadable resources from files, you must use the **full path** to the resource in question. I didn't realize this at first, and it got me quite frustrated until I used a full path to a resource and it worked. Just some friendly tips for you >;3
