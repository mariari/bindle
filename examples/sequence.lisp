(defpackage :sequence
  (:use #:cl #:module))

(in-package :sequence)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmodule step struct ()
             (defclass t ()
               ;; need a sum type here, not sure how to do it
               ))
  
  (defmodule expert struct ()
             (defun next-step (seq))

             (defun delayed-fold-step (s &key init f finish)
               (labels
                   ((rec (s next finish f acc)
                      (match-seq (funcall next s)
                                 :done  (lambda ()    (funcall finish acc))
                                 :skip  (lambda (s)   (funcall f acc () (lambda (x) (rec s next finish f x))))
                                 :yeild (lambda (a s) (funcall f acc a  (lambda (x) (rec s next finish f x)))))))
                 (rec (sequence-s s) (sequence-next s) finish f init)))))

(defun unfold-step (init f)
  (make-sequence :init init
                 :f f))

(defun unfold (init f)
  (unfold-step init
               (lambda (s) (let* ((res (funcall f s)))
                        (if (null res)
                            step.*done*
                            (step.yeild RESUME-AT))))))
