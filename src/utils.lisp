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
