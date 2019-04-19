(defpackage :sequence
  (:use #:cl #:module))

(in-package :sequence)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmodule step struct ()
             (defclass t ()))
  
  (defmodule expert struct ()
             (defun next-step seq )))
