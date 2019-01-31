(in-package :cl-module-functor-test)

(def-suite expanders-test
    :description "Tests the expanders package")

(in-suite expanders-test)

(defpackage #:test)

(test make-handler
  (is (equalp
       (expanders:make-handler '(defun test::blah))
       (expanders::make-stop :changed '(defun test::blah))))

  (is (equalp
       (expanders:make-handler '(defun test::blah)
                               :resume-at '(defun baz (x) 3))
       (expanders::make-recursively :changed '(defun test::blah)
                                    :resume-at '(defun baz (x) 3)))))

(test parameter-expander
  (is (equalp
       (funcall (gethash "DEFPARAMETER" expanders::*expander-table*)
                '(defparameter cool 2)
                "TEST")
       (expanders::make-recursively :changed '(DEFPARAMETER TEST::COOL)
                                    :resume-at '(2)))))
