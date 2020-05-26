# picl
### _Anish Moorthy <anlsh@protonmail.com>_

*Python Iterators in Common Lisp. Pronounced like "pickle"*

I aim to provide a complete port of Python's `itertools` package. Other projects
have attempted something similar. I am aware of
- [cl-itertools](https://github.com/mabragor/cl-itertools)
- [snakes](https://github.com/BnMcGn/snakes)

Unfortunately, both have shortcomings. `cl-itertools` is incomplete,
and `snakes` relies on `cl-cont` meaning that it won't necessarily play nice
with the condition system (or so I hear: I know next to nothing about either,
to be honest).

My hope is to reduce fragmentation of the Common Lisp ecosystem by
providing a standard interface to and set of tools for working with streams.

Correctness and extensibility will be prioritized over speed, but I'm not
looking to write a library for snails either. Once an initial release is up and
running I might start slightly deviating from Python's approach, such as by
introducing a `CopyableIterator`subclass to make certain functions more
memory-efficient.

Pull requests and feedback (concerning anything, such as the defragmentation
goal) are welcome.

### Interface

This project implements utilities for working with streams. The interface
is as follows. A stream is any function emitting two values, where the first
is the item of relevance and the second is a boolean indicating whether the
stream is alive. If the alive-indicator is `nil`, then the `item` is ignored.

As an example, an iterator over `'(1 2)'` produces
`1, t` then` 2, t` then `nil, nil`: *not* `1, t` then `2, nil`

Once a stream has indicated that it is dead, all further calls should also
indicate likewise.

### Stability

This project is currently unreleased so I won't garuntee stability just yet:
it's not far off though.

### A note on `:use`

This is a new library, and so I have the liberty to tell you this: do not, and I
mean *do not* `:use` this package. This library might export new symbols in the
future, and I do not care about accomodating the use of `:use`.

Use a [package local
nicknames]((https://gist.github.com/phoe/2b63f33a2a4727a437403eceb7a6b4a3))
instead. It would also be cool if you could include similar disclaimers in any
new CL packages you release and stop `:use`ing things going forward.

### To-do before release

- Implement the remaining itertools functions: groupby, zip_longest
- Provide a driver for ITERATE
- Figure out how to handle `accumulate`(possibly involving a dependency
on `generic-cl` for its `reduce` function)
- Documentation
- Code review

### Lower-priority to-dos (mb after release)
- Port the more-itertools recipes found at bottom of the Python itertools
package
- Port the [more-iterools](https://pypi.org/project/more-itertools/) package
(this might be a big job)

### Acknowledgements

Python, its itertools package, and all those who have contributed to it:
without whom this package would not exist

## License

This project is provided under the MIT License (see LICENSE.md)
