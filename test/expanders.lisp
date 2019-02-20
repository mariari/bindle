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
       (funcall (gethash :defparameter expanders::*expander-table*)
                '(defparameter cool 2)
                'test
                bindle.set:+empty+)
       (expanders::make-recursively :changed '(defparameter test::cool)
                                    :resume-at '(2)
                                    :export '(test::cool))))
  (is (equalp
       (funcall (gethash :defclass expanders::*expander-table*)
                '(defclass name ()
                  ((name :accessor name :reader read-name :writer set-name)
                   lisp))
                'test
                bindle.set:+empty+)
       (expanders::make-stop
        :changed '(defclass test::name ()
                   ((name :accessor test::name :reader test::read-name
                     :writer test::set-name)
                    lisp))
        :export '(test::set-name test::read-name test::name test::name)))))

;; Add these tests later

;; (let*-handler '(let* ((cl-user::a 2) (b a)) b) 'test bindle.set:+empty+)
;; (let*-handler '(let* ((cl-user::a 2) (b cl-user::a)) b) 'test bindle.set:+empty+)
;; (let*-handler '(let* ((a 2) (b a)) b) 'test bindle.set:+empty+)

;; (flet-handler '(labels ((blah (x) (if (zerop x) 1 (foo (1- x))))
;;                                      (foo (x)  (if (zerop x) 1 (blah (1- x)))))
;;                              (blah 2)) 'test bindle.set:+empty+)
