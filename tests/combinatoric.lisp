(in-package :picl/tests)
(f:in-suite picl-test-suite)

(defun lexic-lt (i l1 l2)
  (if (= i (length l1) (length l2))
      t
      (if (= (aref l1 i) (aref l2 i))
          (lexic-lt (1+ i) l1 l2)
          (< (aref l1 i) (aref l2 i)))))

(defun enums-lexic (num-things thing-len ls)
  (loop with set = (gcl:make-hash-set)
        for l on ls
        do (if (gcl:memberp (car l) set)
               (return nil)
               (gcl:nadjoin (car l) set))
        when (/= (length (car l)) thing-len)
          do (return nil)
        when (cdr l)
          do (unless (lexic-lt 0 (car l) (cadr l))
               (return nil))
        finally (return (= num-things (length (gcl:map-keys set))))))

;; Algorithmic tests for the combinatorial iterators. These check to make sure
;; that the proper number of results are emitted and that they are in lexicographic
;; order (which is transitive, thankfully)
;; This *should* ensure that the functions function properly (at least on the input
;; sizes where we check). However in case there are any bugs, we also have hardcoded
;; tests below

(f:def-test test/product ()
  (f:is (loop with num-sets = 3
              for setsize below 6
              always (enums-lexic (expt setsize num-sets) num-sets
                                  (iter-to-list (apply #'product (loop for _ below num-sets
                                                                       collect (range 0 setsize)))))))
  (f:is (loop for num-sets from 1 below 6
              with setsize = 3
              always (enums-lexic (expt setsize num-sets) num-sets
                                  (iter-to-list (apply #'product (loop for _ below num-sets
                                                                       collect (range 0 setsize))))))))

(f:def-test test/permutations ()
  (f:is (loop for n below 7
              always (enums-lexic (alx:factorial n) n
                                  (iter-to-list (permutations (range 0 n)))))))

(f:def-test test/combinations ()
  (f:is (loop for n below 7
              always
              (loop for r upto n
                    always (enums-lexic (alx:binomial-coefficient n r) r
                                        (iter-to-list (combinations (range 0 n) r)))))))

(f:def-test test/combinations-with-rep ()
  (f:is (loop for n below 7
              always
              (loop for r upto n
                    always (enums-lexic (if (zerop n) 1
                                            (alx:binomial-coefficient (+ n r -1) r))
                                        r
                                        (iter-to-list (combinations-with-rep (range 0 n) r)))))))

;; Hardcoded tests for the combinatoric iterators. Readable, but quickly become gigantic
;; when n gets bigger. Therefore the above

(f:def-test test/product/hardcoded ()
  (let ((a (make-iterator '(1 2 3 4)))
        (b (make-iterator nil))
        (c (make-iterator '(5 6 7 8))))
    (f:is (iter-makes nil (product a b c)))
    (f:is (and (iter-makes nil a) (iter-makes nil b) (iter-makes nil c)))
    (f:is (iter-makes (list #(1))
                      (product '(1))))
    (f:is (iter-makes (list #(1 2))
                      (product '(1) '(2))))
    (f:is (iter-makes (list #(1 2) #(1 3))
                      (product '(1) '(2 3))))
    (f:is (iter-makes (list #(1 3) #(2 3))
                      (product '(1 2) '(3))))
    (f:is (iter-makes (list #(1 10 100) #(1 10 200) #(1 10 300)
                            #(1 20 100) #(1 20 200) #(1 20 300)
                            #(1 30 100) #(1 30 200) #(1 30 300)
                            #(2 10 100) #(2 10 200) #(2 10 300)
                            #(2 20 100) #(2 20 200) #(2 20 300)
                            #(2 30 100) #(2 30 200) #(2 30 300)
                            #(3 10 100) #(3 10 200) #(3 10 300)
                            #(3 20 100) #(3 20 200) #(3 20 300)
                            #(3 30 100) #(3 30 200) #(3 30 300))
                      (product '(1 2 3) '(10 20 30) '(100 200 300))))))

(f:def-test test/permutations/hardcoded ()
  (f:is (iter-makes (list #()) (permutations nil)))
  (f:is (iter-makes (list #(1)) (permutations '(1))))
  (f:is (iter-makes (list #(1 2) #(2 1))
                    (permutations '(1 2))))
  (f:is (iter-makes (list #(1 2 3) #(1 3 2)
                          #(2 1 3) #(2 3 1)
                          #(3 1 2) #(3 2 1))
                    (permutations '(1 2 3))))
  (f:is (iter-makes (list #(1 2 3 4) #(1 2 4 3) #(1 3 2 4) #(1 3 4 2) #(1 4 2 3) #(1 4 3 2)
                          #(2 1 3 4) #(2 1 4 3) #(2 3 1 4) #(2 3 4 1) #(2 4 1 3) #(2 4 3 1)
                          #(3 1 2 4) #(3 1 4 2) #(3 2 1 4) #(3 2 4 1) #(3 4 1 2) #(3 4 2 1)
                          #(4 1 2 3) #(4 1 3 2) #(4 2 1 3) #(4 2 3 1) #(4 3 1 2) #(4 3 2 1))
                    (permutations '(1 2 3 4))))
  (let ((5p (iter-to-list (permutations '(1 2 3 4 5)))))
    (f:is (= (* 5 4 3 2 1) (length 5p)))
    (f:is (equalp #(5 4 3 2 1) (car (last 5p)))))
  (f:is (iter-makes (list #(1) #(2) #(3) #(4) #(5))
                    (permutations '(1 2 3 4 5) 1)))
  (f:is (iter-makes (list #(1 2) #(1 3) #(1 4)
                          #(2 1) #(2 3) #(2 4)
                          #(3 1) #(3 2) #(3 4)
                          #(4 1) #(4 2) #(4 3))
                    (permutations '(1 2 3 4) 2)))
  (f:is (iter-makes (list #(1 2 3) #(1 2 4) #(1 3 2) #(1 3 4) #(1 4 2) #(1 4 3)
                          #(2 1 3) #(2 1 4) #(2 3 1) #(2 3 4) #(2 4 1) #(2 4 3)
                          #(3 1 2) #(3 1 4) #(3 2 1) #(3 2 4) #(3 4 1) #(3 4 2)
                          #(4 1 2) #(4 1 3) #(4 2 1) #(4 2 3) #(4 3 1) #(4 3 2))
                    (permutations '(1 2 3 4) 3))))

(f:def-test test/combinations/hardcoded ()
  (f:is (iter-makes (list #(1) #(2) #(3) #(4) #(5))
                    (combinations '(1 2 3 4 5) 1)))
  (f:is (iter-makes (list #(1 2) #(1 3) #(1 4) #(1 5)
                          #(2 3) #(2 4) #(2 5)
                          #(3 4) #(3 5)
                          #(4 5))
                    (combinations '(1 2 3 4 5) 2)))
  (f:is (iter-makes (list #(1 2 3) #(1 2 4) #(1 2 5) #(1 3 4) #(1 3 5) #(1 4 5)
                          #(2 3 4) #(2 3 5) #(2 4 5)
                          #(3 4 5))
                    (combinations '(1 2 3 4 5) 3)))
  (f:is (iter-makes (list #(1 2 3 4) #(1 2 3 5) #(1 2 4 5) #(1 3 4 5)
                          #(2 3 4 5))
                    (combinations '(1 2 3 4 5) 4)))
  (f:is (iter-makes (list #(1 2 3 4 5))
                    (combinations '(1 2 3 4 5) 5))))
