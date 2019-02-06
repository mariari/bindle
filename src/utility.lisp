(defpackage #:utility
  (:documentation "symbol utilities for better dealing with modules")
  (:use #:cl)
  (:export #:intern-sym
           #:package-designator
           #:on-car
           #:group))

(in-package #:utility)

;;;; Types--------------------------------------------------------------------------------
(deftype package-designator ()
  "package designator, taken from intern... note that this is used in SBCL
source code but is not exposed"
  `(or (vector character)
       (vector nil)
       base-string symbol
       character package))

;;;; Functions----------------------------------------------------------------------------
(declaim (ftype (function (symbol &optional package-designator)
                          (values symbol (member :internal :external :inherited nil)))
                intern-sym))
(defun intern-sym (sym &optional (package-designator *package*))
  (intern (symbol-name sym) package-designator))

(declaim (ftype (function (function list) list) on-car))
(defun on-car (f xs)
  "applies a function f onto the car of a list"
  (cons (funcall f (car xs)) (cdr xs)))

(declaim (ftype (function (fixnum list) list) group))
(defun group (n xs)
  "groups a list into lists of size n"
  (labels ((rec (i xs acc)
             (cond ((null xs) (reverse (on-car #'reverse acc)))
                   ((zerop i) (rec (1- n)
                                   (cdr xs)
                                   (cons (list (car xs))
                                         (on-car #'reverse acc))))
                   (t         (rec (1- i)
                                   (cdr xs)
                                   (on-car (lambda (as) (cons (car xs) as)) acc))))))
    (rec n xs '())))

