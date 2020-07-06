;; The source code for all of these is translated from the example
;; implementations from Python's Standard library
;; https://docs.python.org/3.9/library/itertools.html

(in-package :picl)

(def-iter iterator-product (item-vec indices lengths stopped)

    (product (&rest iterables)
      "Cartesian product of input iterables, returned as vectors in lexicographic order.

Due to the awkwardness in mixing `&rest` and `&key` parameters in lambda lists, this function
does not implement the `repeat` argument supported in
[Python's version](https://docs.python.org/3/library/itertools.html#itertools.product).
Use `picl:nfold-product` instead.

```
(product '(1 2) '(3 4))
;; #(1 3), #(1 4), #(2 3), #(2 4)
```"
      (loop with item-vec = (make-array (length iterables))
            with lengths = (make-array (length iterables))
            for i below (length item-vec)
            for iter in iterables
            do (setf (aref item-vec i) (iter-to-vec iter))
               (setf (aref lengths i) (length (aref item-vec i)))
            minimizing (aref lengths i) into min-len
            finally
               (return (if (zerop min-len)
                           (empty-iterator)
                           (init-state item-vec
                                       lengths
                                       (indices (make-array (length item-vec)
                                                            :initial-element 0)))))))
  (if stopped
      (values nil nil)
      (multiple-value-prog1
          (loop with product = (make-array (length indices))
                for i below (length indices)
                for ii = (aref indices i)
                do (setf (aref product i) (aref (aref item-vec i) ii))
                finally (return (values product t)))
        (labels ((next-combo (i)
                   (if (< i 0)
                       (setf stopped t)
                       (progn
                         (incf (aref indices i))
                         (when (>= (aref indices i) (aref lengths i))
                           (setf (aref indices i) 0)
                           (next-combo (1- i)))))))
          (next-combo (1- (length indices)))))))

(def-iter iterator-nfold-product (iterable-length product-length item-vec indices stopped)
    (nfold-product (n iterable)
      "Computes the n-fold Cartesian product of an iterable with itself.

Essentially equivalent to `(apply #'product (iter-to-list (tee n iterable))`, but with
much better memory usage

```
(nfold-product 3 '(1 2))
;; #(1 1 1), #(1 1 2), #(1 2 1), #(1 2 2), #(2 1 1), #(2 1 2), #(2 2 1), #(2 2 2)
```
"
      (let* ((item-vec (iter-to-vec iterable))
             (iterable-length (length item-vec)))
        (if (zerop iterable-length)
            (empty-iterator)
            (init-state iterable-length item-vec (product-length n)
                        (indices (make-array n :initial-element 0))))))
  (if stopped
      (values nil nil)
      (multiple-value-prog1
          (loop with product = (make-array product-length)
                for i below product-length
                for ii = (aref indices i)
                do (setf (aref product i) (aref item-vec ii))
                finally (return (values product t)))
        (labels ((next-combo (i)
                   (if (< i 0)
                       (setf stopped t)
                       (progn
                         (incf (aref indices i))
                         (when (>= (aref indices i) iterable-length)
                           (setf (aref indices i) 0)
                           (next-combo (1- i)))))))
          (next-combo (1- product-length))))))

;; Permutations

(def-iter iterator-permutations (r n pool indices stopped cycles)

    (permutations (iterable &optional r)
      "`r`-permutations of input iterable, returned as vectors in lexicographic order.

If `r` is not given, it defaults to the length of the input iterable

```
(permutations '(1 2 3))
;; #(1 2 3), #(1 3 2), #(2 1 3), #(2 3 1), #(3 1 2), #(3 2 1)
(permutations '(1 2 3) 2)
;; #(1 2), #(1 3), #(2 1), #(2 3), #(3 1), #(3 2)
```"
      (let* ((ivec (iter-to-vec iterable))
             (n (length ivec))
             (r (or r n)))
        (if (> r n)
            (empty-iterator)
            (init-state r n (pool ivec)
                        (indices (iter-to-vec (range 0 (length ivec) 1)))
                        (cycles (iter-to-vec (range n (- n r) -1)))))))
  (if stopped
      (values nil nil)
      (multiple-value-prog1
          (loop with ret = (make-array r)
                for i below r
                do (setf (aref ret i) (aref pool (aref indices i)))
                finally (return (values ret t)))
        (loop for i from (1- r) downto 0
              do (decf (aref cycles i))
                 (if (zerop (aref cycles i))
                     (progn (loop with curr = (aref indices i)
                                  for j from i below (1- n)
                                  do (setf (aref indices j) (aref indices (1+ j)))
                                  finally (setf (aref indices (1- n)) curr))
                            (setf (aref cycles i) (- n i)))
                     (let ((j (aref cycles i)))
                       (rotatef (aref indices i) (aref indices (- n j)))
                       (return)))
              finally (setf stopped t)))))

;; Combinations
(def-iter iterator-combinations (indices pool stopped r n)

    (combinations (r iterable)
      "r-combinations of input iterable, returned as vectors in lexicographic order.

```
(combinations 2 '(1 2 3))
;; #(1 2), #(1 3), #(2 3)
```"
      (let ((ivec (iter-to-vec iterable)))
        (if (> r (length ivec))
            (empty-iterator)
            (init-state r (n (length ivec)) (pool ivec)
                        (indices (iter-to-vec (range 0 r 1)))))))

  (if stopped
      (values nil nil)
      (multiple-value-prog1
          (loop with ret-vec = (make-array r)
                for i below r
                do (setf (aref ret-vec i) (aref pool (aref indices i)))
                finally (return (values ret-vec t)))
        (loop for i from (1- r) downto 0
              when (/= (aref indices i) (+ i n (- r)))
                do (incf (aref indices i))
                   (loop for j from (1+ i) to (1- r)
                         do (setf (aref indices j) (1+ (aref indices (1- j)))))
                   (return)
              finally (setf stopped t)))))

(def-iter iterator-combinations-with-rep (indices pool stopped r n)

    (combinations-with-rep (r iterable)
      "r-combinations with replacement of input iterable, returned as vectors in lexicographic
order

```
(combinations 2 '(1 2 3))
;; #(1 1), #(1 2), #(1 3), #(2 1), #(2 2), #(2 3), #(3 1), #(3 2), #(3 3)
```"
      (let ((pool (iter-to-vec iterable)))
        (init-state r
                    (n (length pool))
                    (pool pool)
                    (indices (iter-to-vec (repeat 0 r))))))

  (if stopped
      (values nil nil)
      (multiple-value-prog1
          (loop with ret-vec = (make-array r)
                for i below r
                do (setf (aref ret-vec i) (aref pool (aref indices i)))
                finally (return (values ret-vec t)))
        (loop for i from (1- r) downto 0
              when (/= (aref indices i) (1- n))
                do
                   (loop for j from i below r
                         with el = (1+ (aref indices i))
                         do (setf (aref indices j) el))
                   (return)
              finally (setf stopped t)))))
