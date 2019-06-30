(defpackage :inner-modules
  (:use #:cl #:module))

(in-package :inner-modules)

(defmodule testf struct ()
  (defun test (x) (+ x 2))
  (defmodule testg struct ()
    (defun test-2 (x)
      (test (+ x 3)))
    (defparameter *x* (test-2 3)))
  (defun tests (x) (+ x 2))
  (+ testg.*x* 3))


(print (testf.testg:test-2 3))
