(defpackage :inner-modules
  (:use #:cl #:module))

(in-package :inner-modules)

(defmodule testfg2 struct ()
  (defmodule test2fg2 struct ()
    (defun test-2 (x)
      (test (+ x 3))))
  (defun test (x) (+ x 2)))
