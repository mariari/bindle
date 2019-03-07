(defpackage #:utility
  (:documentation "symbol utilities for better dealing with modules")
  (:use #:cl)
  (:export #:intern-sym
           #:intern-sym-curr-package
           #:curr-packagep
           #:package-designator
           #:on-car
           #:group
           #:foldl-map
           #:concat-symbol
           #:let-alias
           #:let-alias-m
           #:let-alias-v))

(in-package #:utility)

;;;; Types--------------------------------------------------------------------------------
(deftype package-designator ()
  "package designator, taken from intern... note that this is used in SBCL
source code but is not exposed"
  `(or (vector character)
       (vector nil)
       base-string symbol
       character package))

;;;; Functions----------------------------------------------------------------------------
(declaim (ftype (function (symbol &optional package-designator)
                          (values symbol (member :internal :external :inherited nil)))
                intern-sym))
(defun intern-sym (sym &optional (package-designator *package*))
  (intern (symbol-name sym) package-designator))

(declaim (ftype (function (symbol &optional package-designator)
                          (values symbol (member :internal :external :inherited nil)))
                intern-sym-curr-package))
(defun intern-sym-curr-package (sym &optional (package-designator *package*))
  "works like intern-sym but only interns the symbol if it's in the current package"
  (if (curr-packagep sym)
      (intern-sym sym package-designator)
      sym))

(defun curr-packagep (sym)
  (or (eq (symbol-package sym) *package*)
      (multiple-value-bind (_ b)  (find-symbol (symbol-name sym))
        (declare (ignore _))
        (eq :INHERITED b))))

(declaim (ftype (function (function list) list) on-car))
(defun on-car (f xs)
  "applies a function f onto the car of a list"
  (cons (funcall f (car xs)) (cdr xs)))

(declaim (ftype (function (fixnum list) list) group))
(defun group (n xs)
  "groups a list into lists of size n"
  (labels ((rec (i xs acc)
             (cond ((null xs) (reverse (on-car #'reverse acc)))
                   ((zerop i) (rec (1- n)
                                   (cdr xs)
                                   (cons (list (car xs))
                                         (on-car #'reverse acc))))
                   (t         (rec (1- i)
                                   (cdr xs)
                                   (on-car (lambda (as) (cons (car xs) as)) acc))))))
    (rec n xs '())))


(defun foldl-map (f init xs)
  (let* ((acc init)
         (new-list
          (mapcar (lambda (x)
                    (let ((acc-syntax (funcall f acc x)))
                      (setf acc (car acc-syntax))
                      (cadr acc-syntax)))
                  xs)))
    (list acc new-list)))

(declaim (ftype (function (symbol symbol) string) update-inner-module-name))
(defun concat-symbol (prefix symbol)
  (intern (concatenate 'string (symbol-name prefix) "." (symbol-name symbol))))

;; (let-alias foo cl-user ((+ bin baz) -)
;;   (foo.+ 2 (foo.- 3 4)))

(defmacro let-alias (prefix namespace forms &body body)
  "crates an alias for functions in one namespace to another locally"
  (let ((args (gensym)))
    `(let ,(mapcar (lambda (x)
                     (let ((name (if (listp x) (car x) x)))
                       `(,(concat-symbol prefix name)
                          (symbol-function
                           (find-symbol ,(symbol-name name)
                                        ,namespace)))))
                   forms)
       (flet ,(mapcar (lambda (x)
                        (let ((name (concat-symbol prefix (if (listp x) (car x) x))))
                          `(,name
                            ,(if (consp x)
                                 `(,@(cdr x))
                                 `(&rest ,args))
                            ,(if (consp x)
                                 `(funcall ,name ,@(cdr x))
                                 `(apply ,name ,args)))))
                      forms)
         (declare (ignorable
                   ,@(mapcar (lambda (x) (list 'function (concat-symbol prefix (if (listp x) (car x) x))))
                             forms)))
         ,@body))))

(defmacro let-alias-v (prefix namespace forms &body body)
  "crates an alias for variables in one namespace to another locally"
  `(let ,(mapcar (lambda (x)
                   `(,(concat-symbol prefix x)
                      (find-symbol ,(symbol-name x)
                                   ,namespace)))
                 forms)
     (declare (ignorable
                 ,@(mapcar (lambda (x) (concat-symbol prefix x)) forms)))
     ,@body))

;; (let-alias-m foo cl-user (cond let) (foo.cond (t 2)))
(defmacro let-alias-m (prefix namespace forms &body body)
  (let ((bod (gensym)))
    `(macrolet ,(mapcar (lambda (x)
                          (let ((name (concat-symbol prefix x)))
                            `(,name (&body ,bod)
                                    `(,(find-symbol ,(symbol-name x) ,namespace) ,@,bod))))
                        forms)
       (declare (ignorable
                 ,@(mapcar (lambda (x) (concat-symbol prefix x)) forms)))
       ,@body)))
