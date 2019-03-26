(defpackage :applicative
  (:use #:cl #:module))

(in-package :applicative)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmodule *basic* sig
    (fun return ts)
    (fun app f-app app)
    (val map))

  (defmodule *monad* sig
    (fun return m)
    (fun bind f ms)
    (fun map  f ms))

  (defmodule *app* sig
    return app map mapn))

(eval-when (:compile-toplevel :execute)

  ;; currently don't have module includes... could easily derive it!
  (defmodule make ((mod *basic*)) *app*
    (defun return (ts)
      (mod.return ts))

    (defun app (f-app app)
      (mod.app f-app app))

    (defun derived-map (f ts)
      (app (return f) ts))

    (defun map (f xs)
      (if (eq :custom (car mod.map))
          (funcall (cadr mod.map) f xs)
          (derived-map f xs)))

    (defun mapn (f arg1 &rest args)
      (reduce (lambda (acc x) (app acc x)) args :initial-value (map f arg1))))

  ;; we need to give *app* for it to export the nested funcall call, for whatever reason
  (defmodule of-monad ((M *monad*)) *app*
    (make module:*functor-name*
          (defmodule struct *basic*
            (defun return (x)
              (M.return x))

            (defun app (f-app app)
              (M.bind (lambda (f) (M.map f app)) f-app))

            (defparameter map (list :custom #'M.map)))))

  (defmodule list-monad struct *monad*
    (defun map (f xs)
      (mapcar f xs))

    (defun bind (f xs)
      (mapcan f xs))

    (defun return (x) (list x)))

  (of-monad 'list-app 'list-monad))

(print (list-app:app (list #'1+ (lambda (x) (* 2 x))) (list 1 2 3)))
