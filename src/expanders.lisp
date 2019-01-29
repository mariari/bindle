(defpackage #:expanders
  (:documentation "Gives the ability to write custom expanders so that defmodules
can properly export the symbols to the right namespace")
  (:use #:cl)
  (:export))


(defvar *expander-table*
  (make-hash-table :test 'equal)) ; we use equal for the test as we have to convert symbols to strings


;; write functions that changes defparameter, defvar, defun, defstruct, defclass... etc etc
