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

(assert
 (=
  (alias:with-alias-name reference r
    (let ((z (R.ref 2))
          (y (R.ref 3)))
      (+ (R.! z) (R.! y))))
  5))

(assert
 (=
  (alias:with-alias-name cl c
    (c.defun f () (+ 2 3))
    (let ((z (c.+ 2 3))
          (y (c.* 2 4)))
      (c.+ z y)))
  13))
