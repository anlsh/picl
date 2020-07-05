# Important Note: Pre-Release

  I am currently soliciting feedback on the codebase, and won't garuntee the
    stability of any existing functionality yet. With that said, I don't expect
    this to last too long or for the API to change that much

# picl
#### _Anish Moorthy <anlsh@protonmail.com>_

*Python Itertools in Common Lisp. Pronounced like "pickle"*

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

### Documentation
All functions are annotated with markdown docstrings. Thanks to Shinmera's
[Staple](https://github.com/Shinmera/staple) and [htmlpreview](https://github.com/htmlpreview/htmlpreview.github.com),
you can view the [documentation here](https://htmlpreview.github.io/?https://github.com/anlsh/picl/blob/master/docs/index.html)

(Yes, I'm aware there's some sort of javascript garbage at the bottom of the
  page: it seems like some a bug with htmlpreview:. It doesn't truncate the
  documentation however, so I suppose this is just the price I pay for not
  knowing how to host webpages myself)

You can build the documentation yourself like so

``` common-lisp
(staple:generate :picl :if-exists :overwrite)
```

If you don't have the `iterate` package loaded it'll complain about invalid
function names, but you can ignore that.

### Testing
A fairly comprehensive test suite is provided using
[FiveAM](https://common-lisp.net/project/fiveam/).

You can run the test suite yourself like so

``` common-lisp
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

Use [package local
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
