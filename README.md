# picl
### _Anish Moorthy <anlsh@protonmail.com>_

*Python Iterators in Common Lisp. Pronounced like "pickle"*

I aim to provide a complete port of Python's `itertools` package. Other projects
have attempted something similar. I am aware of
- [cl-itertools](https://github.com/mabragor/cl-itertools)
- [snakes](https://github.com/BnMcGn/snakes)

Unfortunately, both have shortcomings. `cl-itertools` is incomplete,
and `snakes` relies on `cl-cont` meaning that it won't necessarily play nice
with the condition system (or so I'm told: I know next to nothing about either,
to be honest).

My hope is actually to *reduce* fragmentation of the Common Lisp ecosystem by
creating this package. My current approach is to rip off what Python's
interface, which consists of `__iter__`, `__next__`, and a`StopIteration`
exception.

Correctness and extensibility will be prioritized over speed, but I'm not
looking to write a library for snails either. Once an initial release is up and
running I might start slightly deviating from Python's approach, such as by
introducing a `CopyableIterator`subclass to make certain functions more
memory-efficient.

Pull requests and feedback (concerning anything, such as the defragmentation
goal) are welcome.

### Stability

For the time being, this project's interface is very much unstable: even the
package name might change (it seems a little cutesy to me. Then again, maybe
that's not such a bad thing) Hopefully it won't be this way for long: if you
would like to see it evolve in a certain direction, feel free to create an issue
or PR.

### A note on `:use`

This is a new library, and so I have the liberty to tell you this: do not, and I
mean *do not* `:use` this package. Even after the library becomes stable I
might want to add functionality involving exporting new symbols and I absolutely
do not care about accomodating the use of `:use`.

Use [package local
nicknames]((https://gist.github.com/phoe/2b63f33a2a4727a437403eceb7a6b4a3))
instead. Even better, include a similar disclaimer in any new CL package you
release and stop `:use`ing things in your own code.

### To-do before release

- Implement the remaining itertools functions: groupby, starmap, zip_longest
[ez but boring]
- Clean up code: name arguments to functions so that they're consistent, figure
out how to handle optional parameters, etc
- Nail down the representation for iterators: lambdas or classes specializing
the "next" function
- Specialize the make-iterator function for arrays
- General proofreading and code review

### Lower-priority to-dos (mb after release)
- Port the more-itertools recipes found at bottom of the Python itertools
package
- Port the [more-iterools](https://pypi.org/project/more-itertools/) package
(this might be a big job)

### Acknowledgements

Python, its itertools package, and all those who have contributed to it:
without whom this package would not exist

## License

MIT
