(in-package :cl-module-functor-test)

(def-suite diff-list-test
    :description "Test the bindle.diff-list package")

(in-suite diff-list-test)

;;; Tests

(defparameter d-list bindle.diff-list::(d-cons 3 (d-cons 2 (d-cons 1 +empty+))))

(test test-diff-list
  (is (equalp
       (list 3 2 1)
       (bindle.diff-list:to-list d-list)))
      
  (is (equalp
       (list 4 3 2 1)
       bindle.diff-list::(to-list (d-cons 4 d-list))))
  
  (is (equalp
       (list 3 2 1 3 2 1)
       bindle.diff-list::(to-list (d-append d-list d-list))))
  
  (is (equalp
       (list 3 2 1 0)
       bindle.diff-list::(to-list (d-snoc d-list 0)))))
