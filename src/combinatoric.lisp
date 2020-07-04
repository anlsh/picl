;; The source code for all of these is translated from the example
;; implementations from Python's Standard library
;; https://docs.python.org/3.9/library/itertools.html

(in-package :picl)

;; Product
;; TODO It's missing the functionality of "repeat" argument

(def-iter iterator-product (item-vec indices lengths stopped)

    (product (&rest iterlikes)
      "Cartesian product of input iterables, returned in lexicographic order.


      For example, @c((product '(1 2) '(3 4))) yields @c(#(1 3), #(1 4), #(2 3), #(2 4))"
      (loop with item-vec = (make-array (length iterlikes))
            with lengths = (make-array (length iterlikes))
            for i below (length item-vec)
            for iter in iterlikes
            do (setf (aref item-vec i) (iter-to-vec iter))
               (setf (aref lengths i) (length (aref item-vec i)))
            minimizing (aref lengths i) into min-len
            finally
               (return (if (zerop min-len)
                           (init-state (stopped t))
                           (init-state item-vec
                                       lengths
                                       (indices (make-array (length item-vec)
                                                            :initial-element 0)))))))
  (if stopped
      (values nil nil)
      (multiple-value-prog1
          (loop with product = (make-array (length indices))
                for i below (length product)
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

;; Permutations

(def-iter iterator-permutations (r n pool indices stopped cycles)

    (permutations (iterlike &optional r)
      (let* ((ivec (iter-to-vec iterlike))
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

    (combinations (iterlike r)
      (let ((ivec (iter-to-vec iterlike)))
        (if (> r (length ivec))
            (empty-iterator)
            (init-state r (n (length ivec)) (pool ivec)
                        (indices (iter-to-vec (range 0 r 1)))))))

  ;; TODO Strictly speaking, this should return a set
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

    (combinations-with-rep (iterlike r)
      (let ((pool (iter-to-vec iterlike)))
        (init-state r
                    (n (length pool))
                    (pool pool)
                    (indices (iter-to-vec (repeat 0 r))))))

  ;; TODO Strictly speaking, this should return a multiset
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
