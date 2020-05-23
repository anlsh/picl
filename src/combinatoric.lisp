;; The source code for all of these is translated from the example
;; implementations from Python's Standard library
;; https://docs.python.org/3.9/library/itertools.html

;; Product
;; TODO It's missing the functionality of "repeat" argument
(dcl:defclass/std iterator-product (iterator)
  ((item-vec state-vec lengths stopped)))

(defun product (&rest iters)
  (loop with iter-vec = (iter-to-vec iters)
        with item-vec = (make-array (length iter-vec))
        with length-vec = (make-array (length iter-vec))
        for i below (length item-vec)
        for iter across iter-vec
        do (setf (aref item-vec i) (iter-to-vec (aref iter-vec i)))
           (setf (aref length-vec i) (length (aref item-vec i)))
        minimizing (aref length-vec i) into min-len
        finally
           (if (zerop min-len)
               (empty-iterator)
               (return (make-instance 'iterator-product
                                      :item-vec item-vec
                                      :state-vec (make-array (length item-vec) :initial-element 0)
                                      :lengths length-vec)))))

(defmethod next ((it iterator-product))
  (with-slots (item-vec state-vec lengths stopped) it
    (if stopped
        (error 'stop-iteration)
        (prog1 (loop with product = (make-array (length state-vec))
                     for i below (length product)
                     for ii = (aref state-vec i)
                     do (setf (aref product i) (aref (aref item-vec i) ii))
                     finally (return product))
          (labels ((next-combo (i)
                     (if (< i 0)
                         (setf stopped t)
                         (progn
                           (incf (aref state-vec i))
                           (when (>= (aref state-vec i) (aref lengths i))
                             (setf (aref state-vec i) 0)
                             (next-combo (1- i)))))))
            (next-combo (1- (length state-vec))))))))

;; Permutations

(dcl:defclass/std iterator-permutations (iterator)
  ((r n pool indices stopped cycles)))

(defun permutations (iterlike &optional r)
  (let* ((ivec (iter-to-vec iterlike))
         (n (length ivec))
         (r (or r n)))
    (if (> r n)
        (empty-iterator)
        (make-instance 'iterator-permutations
                       :indices (iter-to-vec (range 0 (length ivec)))
                       :cycles (iter-to-vec (range n (- n r) :delta -1))
                       :pool ivec
                       :r r :n n))))

(defmethod next ((it iterator-permutations))
  (with-slots (r n pool indices stopped cycles) it
    (if stopped
        (error 'stop-iteration)
        (prog1 (loop with ret = (make-array r)
                     for i below r
                     do (setf (aref ret i) (aref pool (aref indices i)))
                     finally (return ret))
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
                finally (setf stopped t))))))

;; Combinations
(dcl:defclass/std iterator-combinations (iterator)
  ((indices pool stopped r n)))

(defun combinations (iterlike r)
  (let ((ivec (iter-to-vec iterlike)))
    (if (> r (length ivec))
        (empty-iterator)
        (make-instance 'iterator-combinations
                       :indices (iter-to-vec (range 0 r :delta 1))
                       :pool ivec
                       :r r :n (length ivec)))))

;; TODO Strictly speaking, this should return sets: not vectors
(defmethod next ((it iterator-combinations))
  (with-slots (indices pool stopped r n) it
    (when stopped (error 'stop-iteration))
    (prog1 (loop with ret-vec = (make-array r)
                 for i below r
                 do (setf (aref ret-vec i) (aref pool (aref indices i)))
                 finally (return ret-vec))
      (loop for i from (1- r) downto 0
            when (/= (aref indices i) (+ i n (- r)))
              do (incf (aref indices i))
                 (loop for j from (1+ i) to (1- r)
                       do (setf (aref indices j) (1+ (aref indices (1- j)))))
                 (return)
            finally (setf stopped t)))))


(dcl:defclass/std iterator-combinations-with-rep (iterator)
  ((indices pool stopped r n)))

(defun combinations-with-rep (iterlike r)
  (let ((pool (iter-to-vec iterlike)))
    (if (not (and (> r 0) (> (length pool) 0)))
        (empty-iterator)
        (make-instance 'iterator-combinations-with-rep
                       :indices (iter-to-vec (repeat 0 r))
                       :pool pool
                       :r r
                       :n (length pool)))))

;; TODO Strictly speaking, this should return a multiset
(defmethod next ((it iterator-combinations-with-rep))
  (with-slots (indices pool stopped r n) it
    (when stopped (error 'stop-iteration))
    (prog1 (loop with ret-vec = (make-array r)
                 for i below r
                 do (setf (aref ret-vec i) (aref pool (aref indices i)))
                 finally (return ret-vec))
      (loop for i from (1- r) downto 0
            when (/= (aref indices i) (1- n))
              do
                 (loop for j from i below r
                       with el = (1+ (aref indices i))
                       do (setf (aref indices j) el))
                 (return)
            finally (setf stopped t)))))
