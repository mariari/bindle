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
    (fun map  f ms)))

;; currently don't have module includes... could easily derive it!
(defmodule make ((mod *basic*)) ()
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

;; since we don't have nested modules yet, we don't give a signature for what we're given back!!
(defmodule of-monad ((M *monad*)) ()
  (let ((name
         (defmodule struct *basic*
           (defun return (x)
             (M.return x))

           (defun app (f-app app)
             (M.bind (lambda (f) (M.map f app)) f-app))

           (defparameter map (list :custom M.map)))))
    ;; I shouldn't need to multiple-value-bind here
    ;; it should already export to the right name-space
    (multiple-value-bind (mod exps) (funcall make module:*functor-name* name)
      ;; this even though it shows the correct data
      ;; does not export anything to the right namespace...
      ;; just odd, find fix, else I'll need to make an export macro
      (export exps module:*functor-name*)
      mod)))

(defmodule list-monad struct *monad*
  (defun map (f xs)
    (mapcar f xs))

  (defun bind (f xs)
    (mapcan f xs))

  (defun return (x) (list x)))


(funcall of-monad 'list-app 'list-monad)
(print (list-app::return '3))
