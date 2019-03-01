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

(test defparameter-handler
  (is (equalp
       (funcall (expanders:get-handler :defparameter)
                '(defparameter cool 2)
                'test
                expanders::+empty-export-set+)
       (expanders::make-recursively :changed '(defparameter test::cool)
                                    :resume-at '(2)
                                    :export (expanders::make-exports :var '(test::cool))))))

(test class-handler
  (is (equalp
       (funcall (expanders:get-handler :defclass)
                '(defclass name ()
                  ((name :accessor name :reader read-name :writer set-name)
                   lisp))
                'test
                expanders::+empty-export-set+)
       (expanders::make-stop
        :changed '(defclass test::name ()
                   ((name :accessor test::name :reader test::read-name
                     :writer test::set-name)
                    lisp))
        :export (expanders::make-exports
                 :fn '(test::set-name test::read-name test::name)
                 :var '(test::name))))))

(test defun-handler
  (is (equalp
       (funcall (expanders:get-handler :defun)
                '(defun blah (param y &aux (a (car stuff)) (b 2) c) (list x y a b c))
                'test
                expanders::+empty-export-set+)
       (expanders::make-recursively
        :changed '(defun test::blah
                   (test::param test::y
                    &aux (test::a (car stuff)) (test::b 2) test::c))
        :resume-at '((list x y a b c))
        :export (expanders::make-exports :fn '(test::blah))
        :export-local (expanders::make-exports :var '(c b a y param))))))

;; Add these tests later

;; (let*-handler '(let* ((cl-user::a 2) (b a)) b) 'test bindle.set:+empty+)
;; (let*-handler '(let* ((cl-user::a 2) (b cl-user::a)) b) 'test bindle.set:+empty+)
;; (let*-handler '(let* ((a 2) (b a)) b) 'test bindle.set:+empty+)

;; (flet-handler '(labels ((blah (x) (if (zerop x) 1 (foo (1- x))))
;;                                      (foo (x)  (if (zerop x) 1 (blah (1- x)))))
;;                              (blah 2)) 'test bindle.set:+empty+)
