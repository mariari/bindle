(defpackage #:expanders
  (:documentation "Gives the ability to write custom expanders so that defmodules
can properly export the symbols to the right namespace")
  (:use #:cl)
  (:export #:make-handler
           #:add-handler
           #:recusively
           #:stop
           #:stop-p
           #:recursively-p))

(in-package #:expanders)

;;;; Types--------------------------------------------------------------------------------
(deftype recurse-tag ()
  "serves as a recursive tag if the defmodule macro should
resume checking if any expanders need to happen after the given back point"
  `(or (eql :recurse)
       (eql :stop)))

(deftype handle ()
  "Serves as the sum type of recusrively and stop"
  `(or (satisfies recursively-p)
       (satisfies stop-p)))

(defstruct recursively
  "Change syntax up to a point, "
  (changed   '() :type list)
  (resume-at '() :type list))

(defstruct stop
  (changed '() :type list))

;;;; Global expander table----------------------------------------------------------------

;; we use equal for the test as we have to convert symbols to strings
(defvar *expander-table*
  (make-hash-table :test 'equal))

;;;; Functions for the end user to make their own handlers--------------------------------

(declaim (ftype (function (list &key (:resume-at list)) handle) make-handler))
(defun make-handler (changed &key resume-at)
  "makes a handler that either stops at the namespace changes in CHANGED, or
we can hand off this responsibility and let the system resume where you left off
and convert the rest of the syntax!"
  (if resume-at
      (make-recursively :changed changed :resume-at resume-at)
      (make-stop :changed changed)))

(declaim (ftype (function (symbol (function (list utility:package-designator) handle))
                          function)
                add-handler))
(defun add-handler (symbol-trigger trigger)
  "adds a module alias handler to the global table of changing handlers
the SYMBOL-TRIGGER is the symbol you wish for it to go off on. and
TRIGGER is a function which takes a syntax and package and returns a handler"
  (setf (gethash (symbol-name symbol-trigger)
                 *expander-table*)
        trigger))


;;;; Predefined handlers------------------------------------------------------------------
(defun defparamter-handler (syntax package)
  (make-handler (list (car syntax)
                      (utility:intern-sym (cadr syntax) package))
                :resume-at (cddr syntax)))

(add-handler 'defparameter
             #'defparamter-handler)
