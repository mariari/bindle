(defpackage #:bindle.set
  (:documentation "Gives a functional set that has quick copies. Useful for removing
and adding local bindings")
  (:use #:cl)
  (:export #:member
           #:add
           :remove
           #:fset))

(in-package #:bindle.set)

(deftype fset ()
  `list)


;; Use a list for now before Ι Make a functional set
(setf (symbol-function 'add) #'cons)
