(defpackage #:error-type
  (:documentation
   "Provides the error type and utilities thereof")
  (:use #:cl)
  (:export #:either-error
           #:ok-or-error))

(in-package :error-type)

(defun satisfies-error-type (a)
  (case (car a)
    (:ok    t)
    (:error t)
    (t      nil)))

(deftype either-error ()
  "Works like Either in Haskell but encodes the :ok and :error as the
first elements of the list instead of a proper struct type"
  `(satisfies satisfies-error-type))


(declaim (ftype (function (either-error) t) ok-or-error))
(defun ok-or-error (err)
  (if (eq (car err) :error)
      (error (cadr err))
      (cadr err)))
