;;;; TEST MODULE---------------------------------------------------------
(module:defmodule ref struct ()
  (defclass ref ()
    ((contents
      :initarg :contents
      :accessor ref-contents)))

  (defmethod print-object ((obj ref) stream)
      (print-unreadable-object (obj stream :type t)
        (format stream "~a" (ref-contents obj))))

  (defun ref (x)
    "Creates a reference out of x"
    (make-instance 'ref :contents x))

  (defun ! (ref)
    "Grabs the contents of a reference"
    (ref-contents ref))

  (defun := (ref x)
    "sets the reference value to x"
    (setf (ref-contents ref) x))

  ;; Has to come after !, or else the second pass
  ;; doesn't see ! as a function it is
  (defun (setf !) (x ref)
    "sets the reference value to x"
    (:= ref x)))

;;;; TESTS------------------------------------------------------------
;; note I will find a nice testing framework or make one myself
;; later, for now we'll just use boolean tests
;; an expect framework will be made

(in-package :bindle-test)

(def-suite aliasing-test
    :description "Tests the aliasing module")

(in-suite aliasing-test)

(test alias-name
  (for-all ((a (gen-integer))
            (b (gen-integer)))
    (is (=
         (alias:with-alias R Reference
           (let ((z (R.ref a))
                 (y (R.ref b)))
             (+ (R.! z) (R.! y)))))
        (+ a b))

    (is (=
         (alias:with-alias C cl
           (C.flet ((f-test () (C.+ 2 3)))
             (C.let ((z (C.+ a 3))
                     (y (C.* a b)))
               (C.+ z y (f-test)))))
         (+ (+ a 3) (* a b) (+ 2 3))))))

(test let-alias
  (for-all ((a (gen-integer))
            (b (gen-integer))
            (c (gen-integer)))

    (is (=
         (alias:let-alias ((R Reference)
                           (C Cl))
           (let ((z (R.ref a))
                 (y (R.ref b)))
             (C.setf (R.! z) c)
             (C.setf (R.! y) (+ (R.! y) 5))
             (C.+ (R.! z) (R.! y)))))
        (+ c b 5))))
