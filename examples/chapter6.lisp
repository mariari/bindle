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


(defpackage :chapter-6
  (:use #:cl #:module))

(in-package :chapter-6)

;;; General functions---------------------------------------------------------------------------------

(defun call-s (module name &rest args)
  "Finds a function-symbol in a namespace and calls it on args"
  (apply #'funcall
         (find-symbol (symbol-name name) module)
         args))

(defun val-s (module name)
  "Finds a symbol-value in a module"
  (symbol-value (find-symbol (symbol-name name) module)))

;;;; Query Handler-----------------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmodule *query* sig
    create eval-t
    (val name))

  (defmodule *query-instance-handler* sig
    (val query-handler)
    (val this)))

(defmodule unique struct *query*
  (defparameter name "unique")
  (defun create (start-at)
    (reference:ref start-at))

  (defun eval-t (state sexp)
    (let ((val (reference:! state)))
      (if (null sexp)
          (list :ok (prog1 val (setf (reference:! state) (1+ val))))
          (list :error "sexp is not empty")))))

(defmodule list-dir struct *query*
  (defparameter name "ls")
  (defclass cwd ()
    ((val
      :initarg :val
      :accessor cwd-val)))

  (defmethod print-object ((obj cwd) stream)
      (print-unreadable-object (obj stream :type t)
        (format stream "{~a}" (cwd-val obj))))

  (defun create (cwd)
    (make-instance 'cwd :val cwd))

  (defun eval-t (state sexp)
    (if (stringp sexp)
        (let ((dir (if (eq :absolute (car (pathname-directory sexp)))
                       sexp
                       (concatenate 'string (cwd-val state) sexp "/*.*"))))
          (list :ok (directory dir)))
        (list :error "Sexp is not a string"))))

;; needs to be a macro because gensym will fail on us at the top level
(defmacro build-instacne (q config &key (name (gensym)))
  `(defmodule ,name struct *query-instance-handler*
     (defparameter query-handler (find-package ,q))
     (defparameter this (call-s ,q 'create ,config))))

(defun build-dispatch-tables (handlers)
  (let ((table (make-hash-table :test 'equalp)))
    (mapc (lambda (instance)
            (setf (gethash (val-s (val-s instance 'query-handler) 'name)
                           table)
                  instance))
          handlers)
    table))

(defun dispatch (dispatch-table name-and-query)
  (if (and (not (null name-and-query)) (listp name-and-query))
      (let* ((name   (car name-and-query))
             (query  (cadr name-and-query))
             (module (gethash name dispatch-table)))
        (if module
            (call-s (val-s module 'query-handler) 'eval-t
                    (val-s module 'this)
                    query)
            (list :error "Could not find matching handler")))
      (list :error "malformed query")))

(defparameter *unique-instance*   (build-instacne 'unique 0      :name unique-instance))
(defparameter *list-dir-instance* (build-instacne 'list-dir "./" :name dir-instace))

(defparameter *table* (build-dispatch-tables (list *unique-instance* *list-dir-instance*)))

(defmodule fun-test ((foo *query*)) (sig (fun eval))
  (locally (declare #+sbcl(sb-ext:muffle-conditions cl:warning))
    (defparameter *value* (foo.create 0))
    (defun eval ()
      (foo.eval-t *value* '()))))

(fun-test 'baz 'unique)


;; (dispatch *table* (list "ls" "./"))
;; (dispatch *table* (list "unique" '()))

;; Gensym Failure-------------------------------------------------------------------------
;; gensyms on the toplevel does not work too well...
;; see this example here!!!
(defmacro make-gensym ()
  (let ((gensym (gensym)))
    `(list ',gensym)))
;; these 2 gensyms are the same!
(defparameter *gensym1* (make-gensym))
(defparameter *gensym2* (make-gensym))
