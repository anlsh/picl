# picl
#### _Anish Moorthy <anlsh@protonmail.com>_

*Python Iterators in Common Lisp. Pronounced like "pickle"*

An (almost) complete port of Python's
[itertools](https://docs.python.org/3.8/library/itertools.html) package.
Pull requests welcome!

### Objectives and Rationale

I am aware of other libraries providing similar functionality, notably
- [cl-itertools](https://github.com/mabragor/cl-itertools)
- [snakes](https://github.com/BnMcGn/snakes), in addition to providing
  Python-style generators

Unfortunately `cl-itertools` remains very incomplete,
and `snakes` relies on `cl-cont` meaning that it doesn't play nice
with the condition system (apparently).

My hope is to reduce fragmentation of the Common Lisp ecosystem by providing a
library which is complete, performant, and "seamless" (whatever that means
nowadays).

### Documentation & Testing
All functions are annotated with markdown docstrings. Thanks to Shinmera's
[Staple](https://github.com/Shinmera/staple)

### Concepts and How-To

An "iterator" in PICL is simply a function taking no arguments and producing two
values: the payload and the alive-indicator. The payload represent's the
iterator's next item, and the alive-indicator should be truthy until after the
iterator is consumed.

By example

```lisp
(defvar it (make-iterator '(1 2 3)))
(next it) ;; 1 t
(next it) ;; 2 t
(next it) ;; nil nil
```
After returning `nil`, all further `next` calls should also produce `nil` as
quickly as possible. Furthermore when the alive indicator is `nil`, the payload
should be ignored.

To create iterators over your own objects, specialize the `make-iterator`
generic function appropriately to produce a function satisfying the conditions
described above. For instance, the `make-iterator` definition for lists is

``` common-lisp
(defmethod make-iterator ((obj list))
  (lambda ()
    (if obj
        (values (prog1 (car obj) (setf obj (cdr obj))) t)
        (values nil nil))))
```
Specializations for `list` and `vector` are defined in PICL. A universal `in-it`
driver is also provided for [Iterate](https://common-lisp.net/project/iterate/)
through the `picl/iterate` system.

``` common-lisp
(ql:quickload '(:picl :picl/iterate))

(iterate:iter
    (iterate:for i in-it #(1 2 3))
    (iterate:collect i))
;; (1 2 3)
(iterate:iter
    (iterate:for i in-it (picl:permutations '(1 2 3)))
    (iterate:collect i))
;;(#(1 2 3) #(1 3 2) #(2 1 3) #(2 3 1) #(3 1 2) #(3 2 1))
```
though you should probably `:use` iterate so as not to have the `iterate:` prefix
everywhere

#### Missing Functionality
The only functions which are still missing are
[groupby](https://docs.python.org/3.8/library/itertools.html#itertools.groupby)
and
[accumulate](https://docs.python.org/3.8/library/itertools.html#itertools.accumulate).

PICL's product function is missing the `repeat` keyword argument from
[Python's version ](https://docs.python.org/3.8/library/itertools.html#itertools.product).
The same effect can be achieved with `(product (tee iterable n-repeats))`,
though at a higher memory cost

### A note on `:use`

This is a new library, and so I have the liberty to tell you this: do not, and I
mean *do not* `:use` this package. This library might export new symbols in the
future, and I do not care about accomodating the use of `:use`.

Use a [package local
nicknames]((https://gist.github.com/phoe/2b63f33a2a4727a437403eceb7a6b4a3))
instead. It would also be cool if you could include similar disclaimers in any
new CL packages you release and stop `:use`ing things going forward.

### Future Work
- Implement Python's default functions for working with iterables (map, zip, etc)
- Port the more-itertools recipes found at bottom of the Python itertools
package
- Port the [more-iterools](https://pypi.org/project/more-itertools/) package
(this might be a big job)

### Acknowledgements

Python, its itertools package, and all those who have contributed to it.

Shinmera, for creating [Staple](https://github.com/shinmera/staple)

## License

This project is provided under the MIT License (see LICENSE.md)
