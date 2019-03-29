(in-package :bindle-test)

(defun balanced (tree)
  (labels
      ((depth (i tr)
         (if (eq bindle.set:+empty+ tr)
             i
             (let ((depth-l (depth (1+ i) (bindle.set::t-left tr)))
                   (depth-r (depth (1+ i) (bindle.set::t-right tr))))
               (and depth-l
                    depth-r
                    (<= (abs (- depth-l depth-r)))
                    (abs (- depth-l depth-r)))))))
    (depth 0 tree)))

(def-suite set-test
    :description "Tests the bindle.set package")

(in-suite set-test)

(test balance
  (for-all ((elems (gen-integer :max 1000 :min 10)))
    (is (not
         (null
          (balanced (bindle.set:add-seq
                     (loop for i from 0 to elems collect (random 1000000))
                     bindle.set:+empty+)))))))
