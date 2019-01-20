;;;; TEST MODULE---------------------------------------------------------
(defpackage #:reference
  (:documentation "Provides a reference")
  (:use #:cl)
  (:export :ref
           :ref-p
           :!
           ::=))

(in-package reference)

(defstruct ref contents)

(defun ref (x)
  "Creates a reference out of x"
  (make-ref :contents x))

(defun ! (ref)
  "Grabs the contents of a reference"
  (ref-contents ref))

(defun := (ref x)
  "sets the reference value to x"
  (setf (ref-contents ref) x))

(defun (setf !) (x ref)
  "sets the reference value to x"
  (:= ref x))


(in-package cl)

;;;; TESTS------------------------------------------------------------
;; note I will find a nice testing framework or make one myself
;; later, for now we'll just use boolean tests
;; an expect framework will be made

(in-package :cl-module-functor-test)

(def-suite aliasing-test
    :description "Tests the aliasing module")

(in-suite aliasing-test)

(test alias-name
  (for-all ((a (gen-integer))
            (b (gen-integer)))
    (is (=
         (alias:with-alias-name Reference R
           (let ((z (R.ref a))
                 (y (R.ref b)))
             (+ (R.! z) (R.! y)))))
        (+ a b))

    (is (=
         (alias:with-alias-name cl c
           (c.flet ((f-test () (c.+ 2 3)))
             (c.let ((z (c.+ a 3))
                     (y (c.* a b)))
               (c.+ z y (f-test)))))
         (+ (+ a 3) (* a b) (+ 2 3))))))

