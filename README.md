# picl
#### _Anish Moorthy <anlsh@protonmail.com>_

*Python Itertools in Common Lisp (v1.0.0). Pronounced like "pickle"*

A (very nearly) complete port of Python's
[itertools](https://docs.python.org/3.8/library/itertools.html) package,
complete with laziness where applicable.

This project currently lives [on Github](https://github.com/anlsh/picl).
Pull requests welcome!

### Objectives and Rationale

PICL aims to provide a complete port of itertools, complete with laziness,
without any reliance on `cl-cont`.

[cl-itertools](https://github.com/mabragor/cl-itertools)
and [snakes](https://github.com/BnMcGn/snakes), provide similar functionality.
Unfortunately both libraries rely on `cl-cont`, meaning they wont always play
nice with the condition system, and `cl-itertools` remains very incomplete on
top of that


### Installation

PICL is in Quicklisp, and can be installed as follows

``` common-lisp
(ql:quickload :picl)
```

### Documentation
Thanks to [Staple](https://github.com/Shinmera/staple) you can
[read the documentation online](https://anlsh.github.io/picl) or build it
yourself like so

``` common-lisp
(staple:generate :picl :if-exists :supersede)
```
If you don't have PICL's dependencies loaded into your image yet, you'll get
some harmless warnings about invalid definitions

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

An "iterator" in PICL is simply a
[thunk](https://wiki.c2.com/?ProcedureWithNoArguments) producing two values: the
payload and the alive-indicator. The he alive-indicator should be truthy until
*after* the iterator is consumed.

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
generic function appropriately. For instance, the `make-iterator` definition for
lists is

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
    (iterate:for i in-it (picl:permutations '(1 2 3)))
    (iterate:collect i))
;; (#(1 2 3) #(1 3 2) #(2 1 3) #(2 3 1) #(3 1 2) #(3 2 1))
```

### A note on `:use`

Do not `:use` this package. This library might export new symbols in the
future. You have been forewarned.

### Future Work
Missing functions from Python's itertools
- [groupby](https://docs.python.org/3.8/library/itertools.html#itertools.groupby)
- [accumulate](https://docs.python.org/3.8/library/itertools.html#itertools.accumulate)

Extensions to library
- Port the more-itertools recipes found at bottom of the Python itertools
package
- Port the [more-iterools](https://pypi.org/project/more-itertools/) package
(seems like a big job)

## License

This project is provided under the MIT License (see LICENSE.md)
