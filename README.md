# racket-sfml An Object Oriented, kebab case layer for racket-csfml

Why? Because I love sfml, I love racket, I love the way racket does OO and I like consistency
in my code. The csfml syntax does not fit with racket, and also drives me absolutely nuts in general.

It has a good number of helper functions defined because I find them necissery and useful in my own code. If you don't that's fine.
There are also a number of shorthands for certain functions, such as v2f-x for vector2f-x and so on.

If you know sfml/csfml, you shouldn't have too much trouble with this library, though I 
**will** be documenting it when I have time, because it does differ slightly in ways from the C++ version both in
order to conform with racket syntax and maximize ease of use. Many objects and structures and functions have been
renamed for this purpose as well.

As of now, this library is quite operational, but still in development and incomplete in some aspects. Any tweaks !
If you would like to contribute or have any input at all, I'd **love** to take a look at what you have to offer,
or hear you out on what you think this library needs! ^,..,^

If you would like to use it with the `sf:` prefix, simply use `(require sfml/prefixed)`, otherwise, `(require sfml)` provides **almost** everything
you'll need.
