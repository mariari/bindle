(defpackage #:utility
  (:documentation "symbol utilities for better dealing with modules")
  (:use #:cl)
  (:export #:intern-sym
           #:package-designator))

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
