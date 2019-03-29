(in-package :bindle-test)

(def-suite expanders-test
    :description "Tests the expanders package")

(in-suite expanders-test)

(defpackage #:test)

(defun exports-equalp (ex1 ex2)
  (and
   (equalp
    (bindle.diff-list:to-list (expanders::exports-var ex1))
    (bindle.diff-list:to-list (expanders::exports-var ex2)))
   (equalp
    (bindle.diff-list:to-list (expanders::exports-fn ex1))
    (bindle.diff-list:to-list (expanders::exports-fn ex2)))))

(defun handle-equalp (ex1 ex2)
  (let ((t1 (expanders::handle-tag ex1))
        (t2 (expanders::handle-tag ex2)))
    (if (not (equalp t1 t2))
        nil
        (ecase t1
          (:stop (and
                  (equalp (expanders::stop-changed ex1)
                          (expanders::stop-changed ex2))
                  (exports-equalp (expanders::stop-export ex1)
                                  (expanders::stop-export ex2))))
          (:recursively (and (equalp
                              (expanders::recursively-changed ex1)
                              (expanders::recursively-changed ex2))
                             (equalp
                              (expanders::recursively-resume-at ex1)
                              (expanders::recursively-resume-at ex2))
                             (exports-equalp
                              (expanders::recursively-export ex1)
                              (expanders::recursively-export ex2))
                             (exports-equalp
                              (expanders::recursively-export-local ex1)
                              (expanders::recursively-export-local ex2))))))))

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
  (is (handle-equalp
       (funcall (expanders:get-handler :defparameter)
                '(defparameter cool 2)
                'test
                expanders::+empty-export-set+)
       (expanders::make-recursively :changed '(defparameter test::cool)
                                    :resume-at '(2)
                                    :export (expanders::make-exports
                                             :var (bindle.diff-list::of-list '(test::cool)))))))

(test class-handler
  (is (handle-equalp
       (funcall (expanders:get-handler :defclass)
                '(defclass name ()
                  ((name :accessor accessor-name :reader read-name :writer set-name)
                   lisp))
                'test
                expanders::+empty-export-set+)
       (expanders::make-stop
        :changed '(defclass test::name ()
                   ((name :accessor test::accessor-name :reader test::read-name
                     :writer test::set-name)
                    lisp))
        :export (expanders::make-exports
                 :fn (bindle.diff-list::of-list
                      '(test::set-name test::read-name test::accessor-name))
                 :var (bindle.diff-list::of-list
                       '(test::name)))))))

(test defun-handler
  (is (handle-equalp
       (funcall (expanders:get-handler :defun)
                '(defun blah (param y &aux (a (car stuff)) (b 2) c) (list x y a b c))
                'test
                expanders::+empty-export-set+)
       (expanders::make-recursively
        :changed '(defun test::blah
                   (test::param test::y
                    &aux (test::a (car stuff)) (test::b 2) test::c))
        :resume-at '((list x y a b c))
        :export (expanders::make-exports :fn (bindle.diff-list::of-list '(test::blah)))
        :export-local (expanders::make-exports :var (bindle.diff-list::of-list '(c b a y param)))))))

;; Add these tests later

;; (let*-handler '(let* ((cl-user::a 2) (b a)) b) 'test bindle.set:+empty+)
;; (let*-handler '(let* ((cl-user::a 2) (b cl-user::a)) b) 'test bindle.set:+empty+)
;; (let*-handler '(let* ((a 2) (b a)) b) 'test bindle.set:+empty+)

;; (flet-handler '(labels ((blah (x) (if (zerop x) 1 (foo (1- x))))
;;                                      (foo (x)  (if (zerop x) 1 (blah (1- x)))))
;;                              (blah 2)) 'test bindle.set:+empty+)


;; (bindle.diff-list:to-list
;;  (expanders::exports-var
;;   (expanders::stop-export
;;    (expanders::make-stop
;;     :changed '(defclass test::name ()
;;                ((name :accessor test::name :reader test::read-name
;;                  :writer test::set-name)
;;                 lisp))
;;     :export (expanders::make-exports
;;              :fn (bindle.diff-list::of-list
;;                   '(test::set-name test::read-name test::name test::name))
;;              :var (bindle.diff-list::of-list
;;                    '(test::set-name)))))))
