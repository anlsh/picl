(in-package :picl)

;; Utilities
(defun iter-to-list (iterable)
  "Reads `iterable` into a list

```
(iter-to-list (range 4))
;; (0 1 2 3)
(iter-to-list (count))
;; Out of memory error!
```"
  (labels ((rec (iterator)
             (multiple-value-bind (base-item base-alive) (next iterator)
               (if base-alive
                   (cons base-item (iter-to-list iterator))
                   nil))))
    (rec (make-iterator iterable))))

(defun iter-to-vec (iterable)
  "Reads `iterable` into a vector

```
(iter-to-list (range 4))
;; #(0 1 2 3)
(iter-to-list (count))
;; Out of memory error!
```"
  (let ((ls (iter-to-list iterable)))
    (make-array (length ls) :initial-contents ls)))

(defun empty-iterator ()
  "Returns an empty iterator

```
(iter-to-list (empty-iterator))
;; nil
```"
  (make-iterator nil))

(defun take (n iterable)
  "Returns a list consisting of the first `n` (or fewer, if the iterator runs out) items of iterable

```
(take 5 (count))
;; (0 1 2 3 4)
take 30 (range 4)
;; (0 1 2 3)
```"
  (loop with iterator = (make-iterator iterable)
        for i below n
        for (base-item base-alive) = (multiple-value-list (next iterator))
        while base-alive
        collecting base-item into ls
        finally (return ls)))

(defun always (iterable)
  "Truthy iff every element of the argument is truthy

```
(always '(1 2 3))
;; t
(always '(nil))
;; nil
(always nil)
t
```"
  (labels ((always-helper (iterator)
             (multiple-value-bind (payload is-alive) (next iterator)
               (if is-alive
                   (and payload (always-helper iterator))
                   t))))
    (always-helper (make-iterator iterable))))

(defun never (iterable)
  "Truthy iff every element of the argument is nil

```
(never '(1 2 3))
;; nil
(never '(nil))
;; t
(never nil)
t
```"
  (labels ((never-helper (iterator)
             (multiple-value-bind (payload is-alive) (next iterator)
               (if is-alive
                   (and (not payload) (never-helper iterator))
                   t))))
    (never-helper (make-iterator iterable))))

(defun apply (fn &rest args)
  "Like regular apply, except that the final argument can be an arbitrary
iterable.

```
(picl:apply #'+ 1 #(2 3))
;; 6
```
"
  (cl:apply fn (loop for (hd tl) on args
                     appending (if tl (list hd) (picl:iter-to-list hd)))))
