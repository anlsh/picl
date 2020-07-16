# picl
#### _Anish Moorthy <anlsh@protonmail.com>_

*Python Itertools in Common Lisp (v1.0.0). Pronounced like "pickle"*

An (almost) complete port of Python's
[itertools](https://docs.python.org/3.8/library/itertools.html) package,
complete with laziness where applicable.

This project currently lives [on Github](https://github.com/anlsh/picl).
Pull requests welcome!

### Objectives and Rationale

Other libraries, such as
[cl-itertools](https://github.com/mabragor/cl-itertools)
and [snakes](https://github.com/BnMcGn/snakes), provide similar functionality

Unfortunately `cl-itertools` remains very incomplete, and both`cl-itertools`and
`snakes` rely on `cl-cont` meaning that they wont always play nice with certain
parts of CL (apparently)

PICL provides a near-complete port of itertools, complete with laziness, without
any reliance on `cl-cont` or other such libraries

### Installation

PICL isn't in Quicklisp yet, though that should [change
soon](https://github.com/quicklisp/quicklisp-projects/issues/1872). For now,
you'll probably want to clone/symlink it into your `.quicklisp/local-projects`
directory so that you can do a `(ql:quickload :picl)`

### Documentation
All functions are annotated with markdown docstrings. Thanks to
[Staple](https://github.com/Shinmera/staple) you can
[read the documentation online](https://anlsh.github.io/picl) or build it
yourself like so

``` common-lisp
(staple:generate :picl :if-exists :supersede)
```
Just ignore any warnings you get about invalid definitions, all they mean is
that you don't have PICL's dependencies loaded yet.

### Testing
A fairly comprehensive test suite written with
[FiveAM](https://common-lisp.net/project/fiveam/) is provided. You can run it
yourself either manually or through asdf

``` common-lisp
;; The easy way
(asdf:test-system :picl)
;; The slightly less easy way
(ql:quickload :picl/tests)
(fiveam:run! 'picl/tests:suite)
```

### Concepts and How-To

An "iterator" in PICL is simply a function taking no arguments and producing two
values: the payload and the alive-indicator. The payload represent's the
iterator's next item, and the alive-indicator should be truthy until after the
iterator is consumed.

By example

```common-lisp
(let ((it (make-iterator '(1 2))))
  (next it)  ;; (values 1 t)
  (next it)  ;; (values 2 t)
  (next it)) ;; (values nil nil)
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
Specializations for lists and vectors are predefined. A universal `in-it`
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
;; (#(1 2 3) #(1 3 2) #(2 1 3) #(2 3 1) #(3 1 2) #(3 2 1))
```
though you should probably `:use` iterate so as not to have the `iterate:`
prefix everywhere

#### Missing Functionality
The only functions which are still missing are
[groupby](https://docs.python.org/3.8/library/itertools.html#itertools.groupby)
and
[accumulate](https://docs.python.org/3.8/library/itertools.html#itertools.accumulate).

### A note on `:use`

This is a new library, and so I have the liberty to tell you this: do not, and I
mean *do not* `:use` this package. This library might export new symbols in the
future, and I'm not going to accomodate the use of `:use`.

Use [package local
nicknames]((https://gist.github.com/phoe/2b63f33a2a4727a437403eceb7a6b4a3))
instead, and maybe even consider adding similar disclaimers in any new libraries
you release.

### Future Work
- Port the more-itertools recipes found at bottom of the Python itertools
package
- Port the [more-iterools](https://pypi.org/project/more-itertools/) package
(this seems like a big job)

### Acknowledgements

Python, its itertools package, and all those who have contributed to it.

Shinmera, for creating [Staple](https://github.com/shinmera/staple)

## License

This project is provided under the MIT License (see LICENSE.md)
