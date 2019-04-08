(defpackage #:default-expanders
  (:documentation "Gives the ability to write custom expanders so that defmodules
can properly export the symbols to the right namespace")
  (:use #:cl #:expanders)
  (:export
   #:cadr-handler
   #:fn-cadr-handler
   #:var-cadr-handler
   #:*simple-lambda-list-keywords*
   #:lambda-handler
   #:defun-handler
   #:defclass-handler
   #:fns-handler-gen
   #:let-handler-gen
   #:let*-handler
   #:let-handler
   #:labels-handler
   #:flet-handler
   #:module-handler
   #:defgeneric-handler
   #:defmethod-handler
   #:print-unreadable-object-handler
   #:function-handler
   #:setf-handler))

(in-package #:default-expanders)
;;;; Predefined handlers------------------------------------------------------------------
(defun cadr-handler (syntax package change-set fn?)
  "handler that changes the cadr, but keeps the cddr the same"
  (declare (ignore change-set))
  (let ((new-cadr (utility:intern-sym-curr-package (cadr syntax) package)))
    (make-handler (list (car syntax) new-cadr)
                  :export
                  (if fn?
                      (make-exports :fn (bindle.diff-list:of-list (list new-cadr)))
                      (make-exports :var (bindle.diff-list:of-list (list new-cadr))))
                  :resume-at (cddr syntax))))

(defun fn-cadr-handler (syntax package change-set)
  (cadr-handler syntax package change-set t))

(defun var-cadr-handler (syntax package change-set)
  (cadr-handler syntax package change-set nil))

(defvar *simple-lambda-list-keywords* '(&key &optional &aux &rest))

(defun lambda-handler (syntax package change-set)
  (let ((alias (alias-handler-gen* (cadr syntax)
                                   package change-set t
                                   *simple-lambda-list-keywords*)))
    (make-handler (list (car syntax)
                        (alias-changed alias))
                  :export       (alias-export alias)
                  :export-local (alias-export-local alias)
                  :resume-at    (cddr syntax))))

(defun defun-handler (syntax package change-set)
  (let ((new-cadr (cond ((symbolp (cadr syntax))
                         (utility:intern-sym-curr-package (cadr syntax) package))
                        ((export-set-mem-fn (cadadr syntax) change-set)
                         (list (caadr syntax)
                               (utility:intern-sym-curr-package (cadadr syntax) package)))
                        (t
                         (cadr syntax))))
        (alias    (alias-handler-gen* (caddr syntax)
                                      package change-set t
                                      *simple-lambda-list-keywords*)))
    (make-handler (list (car syntax)
                        new-cadr
                        (alias-changed alias))
                  :export       (if (symbolp new-cadr)
                                    (export-fn_ (alias-export alias) new-cadr package)
                                    (alias-export alias))
                  :export-local (alias-export-local alias)
                  :resume-at    (cdddr syntax))))

(defun defclass-handler (syntax package change-set)
  (declare (ignore change-set))
  (let* ((class-name    (utility:intern-sym (cadr syntax) package))
         (super-classes (caddr syntax))
         (slots         (cadddr syntax))
         (options       (cddddr syntax))
         (export        (make-exports :var (bindle.diff-list:of-list (list class-name)))))
    (labels ((handle-slot-options (options)
               (mapcan (lambda (key-default)
                         (if (member (car key-default)
                                (list :accessor :reader :writer)
                                :test #'eq)
                             (let ((new-accessor (utility:intern-sym (cadr key-default) package)))
                               (setf export (export-fn_ export new-accessor package))
                               (list (car key-default) new-accessor))
                             key-default))
                       (utility:group 2 options))))
      (make-handler
       (list* (car syntax)
              class-name
              super-classes
              (mapcar (lambda (accessors)
                        (if (listp accessors)
                            (cons (car accessors) (handle-slot-options (cdr accessors)))
                            accessors))
                      slots)
              options)
       :export export))))


(defun fns-handler-gen (syntax package change-set update?)
  (let* ((fns            (cadr syntax))
         (locally-export (make-exports :fn (bindle.diff-list:of-list
                                            (remove-if-not #'utility:curr-packagep (mapcar #'car fns)))))
         (change-set     (if update?
                             (exports-into-export-set locally-export change-set)
                             change-set))
         (change-fns
          (utility:foldl-map
           (lambda (acc syntax)
             (let*
                 ((alias-args (alias-handler-gen* (cadr syntax)
                                                  package
                                                  change-set t
                                                  *simple-lambda-list-keywords*))
                  (change-set (exports-into-export-set
                               (alias-export alias-args)
                               (car acc)))
                  (params (recursively-change (cddr syntax)
                                              package
                                              (exports-into-export-set
                                               (alias-export-local alias-args)
                                               change-set))))
               (list
                (list (change-params-set params)
                      (join-exports (change-params-exports params)
                                    (alias-export alias-args)
                                    (cadr acc)))
                (list* (utility:intern-sym-curr-package (car syntax) package)
                       (alias-changed alias-args)
                       (change-params-syntax params)))))
           (list change-set +empty-exports+)
           fns)))
    (make-handler (list (car syntax) (cadr change-fns))
                  :export (cadar change-fns)
                  :resume-at (cddr syntax)
                  :export-local locally-export)))

;; just a template to inline the two let-handlers
(defmacro let-handler-gen (f syntax package change-set)
  `(let ((alias (,f (cadr ,syntax) ,package ,change-set)))
     (make-handler (list (car ,syntax) (alias-changed alias))
                   :resume-at    (cddr ,syntax)
                   :export-local (alias-export-local alias)
                   :export       (alias-export alias))))

(defun let*-handler (syntax package change-set)
  (let-handler-gen alias-handler* syntax package change-set))


(defun let-handler (syntax package change-set)
  (let-handler-gen alias-handler syntax package change-set))

(defun labels-handler (syntax package change-set)
  (fns-handler-gen syntax package change-set t))

(defun flet-handler (syntax package change-set)
  (fns-handler-gen syntax package change-set nil))

;; (defun module-handler (syntax package change-set)
;;   (declare (ignore change-set package))
;;   (make-handler syntax))

(defun module-handler (syntax package change-set)
  (declare (ignore change-set))
  (let* ((first-expansion
          (macroexpand-1 syntax))
         (new-package-name
          (concatenate 'string (package-name package)  "." (symbol-name (cadr first-expansion)))))
    ;; TODO: so defun a bunch of functions that one can use in the previous package by using .
    ;; this may be exported by the catch all, but that is fine, however this would let inner modules
    ;; use outer module code, this should be very easy, as we have the 2nd macro expansion, and thus
    ;; we can simply takes the last functions and do a defun alias type of thing!
    (make-handler (macroexpand-1 (list* (car first-expansion)
                                        new-package-name
                                        (cddr first-expansion)))
                  ;; :export (change-params-exports expanded-code)
                  )))

(defun defgeneric-handler (syntax package change-set)
  (declare (ignore change-set))
  (let ((new-cadr (utility:intern-sym-curr-package (cadr syntax) package)))
    (make-handler (list* (car syntax) new-cadr (cddr syntax))
                  :export (export-fn_ +empty-exports+ new-cadr package))))

(defun defmethod-handler (syntax package change-set)
  (let ((new-cadr (export-if-mem-fn (cadr syntax) change-set package))
        (alias    (alias-handler-gen* (caddr syntax)
                                      package change-set t
                                      *simple-lambda-list-keywords*)))
    (make-handler (list (car syntax)
                        new-cadr
                        (alias-changed alias))
                  :export       (alias-export alias)
                  :export-local (alias-export-local alias)
                  :resume-at    (cdddr syntax))))

(defun print-unreadable-object-handler (syntax package change-set)
  (let ((obj-handle (recursively-change `(progn ,@(cadr syntax)) package change-set)))
    (make-handler (list (car syntax) (cdr (change-params-syntax obj-handle)))
                  :resume-at (cddr syntax)
                  :export    (change-params-exports obj-handle))))

(defun function-handler (syntax package change-set)
  (make-handler (list* (car syntax)
                       (export-if-mem-fn (cadr syntax) change-set package)
                       (cddr syntax))))

(defun setf-handler (syntax package change-set)
  (let* ((to-set (cadr syntax))
         (new-to-set
          ;; bit repetitive must be a better way to do this
          (cond ((and (listp to-set) (equal (symbol-name (car to-set)) "SYMBOL-FUNCTION"))
                 (list (car to-set)
                       (utility:intern-sym-curr-package (cadr to-set) package)))
                ((listp to-set)
                 (list (export-if-mem-fn (car to-set) change-set package)
                       (export-if-mem-var (cadr to-set) change-set package)))
                ((export-set-mem-var to-set change-set)
                 (utility:intern-sym-curr-package to-set package))
                (t
                 to-set))))
    (make-handler (list (car syntax) new-to-set)
                  :resume-at (cddr syntax))))

;;; Add handlers--------------------------------------------------------------------------

(add-handler 'print-unreadable-object
             #'print-unreadable-object-handler)

(add-handler 'setf
             #'setf-handler)

;; the list part of the check should just fail
;; and fall through to the t, and work properly
(add-handler 'setq
             #'setf-handler)

(add-handler 'defmodule
             #'module-handler)

(add-handler 'defgeneric
             #'defgeneric-handler)

(add-handler 'defmethod
             #'defmethod-handler)

(add-handler 'labels
             #'labels-handler)

(add-handler 'flet
             #'flet-handler)

(add-handler 'defclass
             #'defclass-handler)

(add-handler 'let*
             #'let*-handler)

(add-handler 'let
             #'let-handler)

(add-handler 'defun
             #'defun-handler)

(add-handler 'lambda
             #'lambda-handler)

(add-handler 'defparameter
             #'var-cadr-handler)

(add-handler 'defvar
             #'fn-cadr-handler)

(add-handler 'deftype
             #'var-cadr-handler)

(add-handler 'function
             #'function-handler)

