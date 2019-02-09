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
                'test)
       (expanders::make-recursively :changed '(DEFPARAMETER TEST::COOL)
                                    :resume-at '(2)
                                    :export '(TEST::COOL))))
  (is (equalp
       (funcall (gethash "DEFCLASS" expanders::*expander-table*)
                '(defclass name ()
                  ((name :accessor name :reader read-name :writer set-name)
                   lisp))
                'test)
       (expanders::make-stop
        :changed '(DEFCLASS TEST::NAME NIL
                   ((NAME :ACCESSOR TEST::NAME :READER TEST::READ-NAME
                     :WRITER TEST::SET-NAME)
                    LISP))
        :export '(TEST::SET-NAME TEST::READ-NAME TEST::NAME TEST::NAME)))))
