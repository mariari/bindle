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
(deftype handle ()
  "Serves as the sum type of recursively and stop. Recursively allows defmodule to continue
where the handler stops, and stop just takes where the handler stops as the full syntax"
  `(or (satisfies recursively-p)
      (satisfies stop-p)))

(defstruct recursively
  "the CHANGED field is aliased augmented syntax to up to a point,
the RESUME-AT field allows defmodule to augment the rest of the given sexp"
  (changed   '() :type list)
  (resume-at '() :type list))

(defstruct stop
  "the CHANGED field is aliased augmented syntax of the entire sexp, defmodule will do no
extra work"
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

;; symbol -> #1=(list -> utility:package-designator -> handle) -> #1#
(declaim
 (ftype (function (symbol #1=(function (list utility:package-designator) handle)) #1#)
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
