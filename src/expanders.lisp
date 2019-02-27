(defpackage #:expanders
  (:documentation "Gives the ability to write custom expanders so that defmodules
can properly export the symbols to the right namespace")
  (:use #:cl)
  (:export #:make-handler
           #:add-handler
           #:recusively
           #:stop
           #:stop-p
           #:recursively-p
           #:get-handler
           #:join-handle
           #:join-exports
           #:+empty-handle+
           #:recursively-change
           #:change-params-set
           #:change-params-exports
           #:change-params-syntax
           #:recursively-change-symbols))


(in-package #:expanders)

;;;; Types--------------------------------------------------------------------------------
(defstruct exports
  "Handles lists of exports, has functions and variables, so one doesn't accidently
change a function when they want to change a variable and vise versa"
  (fn  '() :type list)
  (var '() :type list))

(defstruct export-set
  "Contains two sets, one set that has change functions, and another set that has chagned
variables"
  (fn  bindle.set:+empty+ :type bindle.set:fset)
  (var bindle.set:+empty+ :type bindle.set:fset))

(deftype handle ()
  "Serves as the sum type of recursively and stop. Recursively allows defmodule to continue
where the handler stops, and stop just takes where the handler stops as the full syntax"
  `(or (satisfies recursively-p)
       (satisfies stop-p)))

(defvar +empty-export-set+ (make-export-set
                            :fn bindle.set:+empty+
                            :var bindle.set:+empty+))
(defvar +empty-exports+ (make-exports))

(defstruct recursively
  "the CHANGED field is aliased augmented syntax to up to a point,
the RESUME-AT field allows defmodule to augment the rest of the given sexp
The EXPORT fields tells defmodule what functions should be exported if no signature is given
  (mimicing, no signature export all!!) and the EXPORT-LOCAL states that the bindings are local
to the sexp that is left"
  (changed      '()            :type list)
  (resume-at    '()            :type list)
  (export       +empty-exports+ :type exports)
  (export-local +empty-exports+ :type exports))

(defstruct stop
  "the CHANGED field is aliased augmented syntax of the entire sexp, defmodule will do no
extra work The EXPORT fields tells defmodule what functions should be exported if no
signature is given (mimicing, no signature export all!!)"
  (changed '()            :type list)
  (export  +empty-exports+ :type exports))

(defvar +empty-handle+ (make-stop))

(declaim (ftype (function ((or recursively stop)) keyword) handle-tag))
(defun handle-tag (tag)
  "TAG"
  (cond ((recursively-p tag) :recursively)
        ((stop-p tag)        :stop)))

(defstruct change-params
  "holds the updated changed-set for external definitions, those symbols, so we can pass
exported data, and the updated syntax"
  syntax
  (set     +empty-export-set+ :type export-set)
  (exports +empty-exports+    :type exports))

(defstruct alias
  "Serves as the datastructure returned by the various alias handlers CHANGED
is the changed syntax, EXPORT are the variables that are exported from this syntax
and EXPORT-LOCAL are the variables that are over the next sexp"
  (changed      nil             :type list)
  (export       +empty-exports+ :type exports)
  (export-local +empty-exports+ :type exports))

(defun export-var_ (exp var)
  "A lens to add vars"
  (make-exports :fn (exports-fn exp)
                :var (cons var (exports-var exp))))

(defun export-fns_ (exp fns)
  "A lens to add vars"
  (make-exports :fn  (append (exports-fn exp) fns)
                :var (exports-var exp)))

(defun join-exports (e1 e2 &rest es)
  (labels ((f (e1 e2) (make-exports :fn  (concatenate 'list (exports-fn e1) (exports-fn e2))
                                    :var (concatenate 'list (exports-var e1) (exports-var e2)))))
    (reduce #'f (list* e2 es) :initial-value e1)))

(defun join-handle (a1 a2)
  "Joins two handles into a single one."
  (labels ((concat-fields (fun1 fun2)
             (concatenate 'list (funcall fun1 a1) (funcall fun2 a2))))
    ;; I really wish I had pattern matching
    (ecase (handle-tag a1)
      (:stop
       (ecase (handle-tag a2)
         (:stop
          (make-stop :changed (concat-fields #'stop-changed #'stop-changed)
                     :export  (join-exports (stop-export a1) (stop-export a2))))
         (:recursively
          (join-handle a2 a1))))
      (:recursively
       (ecase (handle-tag a2)
         (:stop
          (make-recursively :changed   (concat-fields #'recursively-changed #'stop-changed)
                            :export       (join-exports (recursively-export a1) (stop-export a2))
                            :resume-at    (recursively-resume-at a1)
                            :export-local (recursively-export-local a1)))
         (:recursively
          (make-recursively :changed      (concat-fields #'recursively-changed #'recursively-changed)
                            :export       (join-exports (recursively-export a1) (recursively-export a2))
                            :resume-at    (concat-fields #'recursively-resume-at #'recursively-resume-at)
                            :export-local (join-exports (recursively-export-local a1)
                                                        (recursively-export-local a2)))))))))

(defun export-set-mem (elem set)
  (or
   (bindle.set:mem elem (export-set-fn set))
   (bindle.set:mem elem (export-set-var set))))

(defun exports-into-export-set (exp set)
  (make-export-set :fn  (bindle.set:add-seq (exports-fn exp)  (export-set-fn set))
                   :var (bindle.set:add-seq (exports-var exp) (export-set-var set))))


;;;; Global expander table----------------------------------------------------------------

;; we use equal for the test as we have to convert symbols to strings
(defvar *expander-table*
  (make-hash-table :test #'equal))

;;;; Functions for dealing with the expander table----------------------------------------

(declaim (ftype (function (list &key (:resume-at list)
                                (:export exports)
                                (:export-local exports))
                          handle)
                make-handler))
(defun make-handler (changed &key (export +empty-exports+) resume-at (export-local +empty-exports+))
  "makes a handler that either stops at the namespace changes in CHANGED, or
we can hand off this responsibility and let the system resume where you left off
and convert the rest of the syntax!"
  (if resume-at
      (make-recursively :changed      changed
                        :export       export
                        :resume-at    resume-at
                        :export-local export-local)
      (make-stop :changed changed
                 :export  export)))

;; symbol -> #1=(list -> utility:package-designator -> handle) -> #1#
(declaim
 (ftype (function (symbol #1=(function (list utility:package-designator export-set) handle)) #1#)
        add-handler))
(defun add-handler (symbol-trigger trigger)
  "adds a module alias handler to the global table of changing handlers
the SYMBOL-TRIGGER is the symbol you wish for it to go off on. and
TRIGGER is a function which takes a syntax and package and returns a handler
the trigger function also takes a set that determines what symbols to export if need be"
  (setf (gethash (utility:intern-sym symbol-trigger 'keyword)
                 *expander-table*)
        trigger))

(defun get-handler (symbol-trigger)
  "Returns the handler corresponding to the SYMBOL-TRIGGER"
  (gethash (utility:intern-sym symbol-trigger 'keyword)
           *expander-table*))

;;;; Functions for the end user to make their own handlers--------------------------------

(defun export-set-var_ (set var)
  (make-export-set :fn (export-set-fn set)
                   :var (bindle.set:add var (export-set-var set))))

;; Note we can compile this more efficiently based on arguments
;; maybe make a macro that does this for me....
;; for example only the last form is used by let and let*
;; Note2: This is only ever used for arguments, so it's safe to 
(defun alias-handler-gen* (syntax package change-set *p &optional ignore)
  (macrolet ((update-utility (symb curr-set changed)
               `(progn
                  (when (utility:curr-packagep ,symb)
                    (when *p
                      (setf ,curr-set (export-set-var_ ,changed ,symb)))
                    (setf export-local (export-var_ export-local ,symb))))))
    (let* ((curr-set     change-set)
           (export-local +empty-exports+)
           (exports      +empty-exports+)
           (change-bindings
            (mapcar
             (lambda (binding-pair)
               (cond
                 ((and (symbolp binding-pair) (member binding-pair ignore))
                  binding-pair)
                 ((symbolp binding-pair)
                  (update-utility binding-pair curr-set curr-set)
                  (utility:intern-sym-curr-package binding-pair package))
                 ;; we have a form like ((:apple a))
                 ((and (listp binding-pair) (listp (car binding-pair)))
                  (let ((changed (recursively-change (cdr binding-pair) package curr-set)))
                    (update-utility (cadar binding-pair)
                                    curr-set
                                    changed)
                    (setf exports (join-exports exports (change-params-exports changed)))
                    (cons (list (caar binding-pair)
                                (utility:intern-sym-curr-package (cadar binding-pair) package))
                          (change-params-syntax changed))))
                 (t
                  (let* ((symb       (car binding-pair))
                         (expression (cdr binding-pair))
                         (changed    (recursively-change expression package curr-set)))
                    (update-utility symb
                                    curr-set
                                    (change-params-set changed))
                    (setf exports (join-exports exports (change-params-exports changed)))
                    (cons (utility:intern-sym-curr-package symb package)
                          (change-params-syntax changed))))))
             syntax)))
      (make-alias :changed      change-bindings
                  :export       exports
                  :export-local export-local))))

(defun alias-handler* (syntax package change-set)
  (alias-handler-gen* syntax package change-set t))

(defun alias-handler (syntax package change-set)
  (alias-handler-gen* syntax package change-set nil))

(declaim (ftype (function (t utility:package-designator export-set) change-params)
                recursively-change))
(defun recursively-change (syntax package change-set)
  "This does the job of defmacro and recursively expands the syntax to what it should be
   keeping in mind what symbols should be changed via change-set.
   Returns back change-params"
  (cond ((and (symbolp syntax)
              (utility:curr-packagep syntax)
              (bindle.set:mem syntax (export-set-var change-set)))
         (make-change-params :syntax (utility:intern-sym syntax package)
                             :set    change-set))
        ((and (listp syntax)
              (symbolp (car syntax))
              (get-handler (car syntax)))
         (let ((handle (funcall (get-handler (car syntax)) syntax package change-set)))
           (ecase (handle-tag handle)
             (:stop
              (make-change-params :syntax  (stop-changed handle)
                                  :exports (stop-export handle)
                                  :set     change-set))
             (:recursively
              (let* ((change-set   (exports-into-export-set (recursively-export handle)
                                                            change-set))
                     (inner-change (recursively-change (recursively-resume-at handle)
                                                       package
                                                       (exports-into-export-set
                                                        (recursively-export-local handle)
                                                        change-set))))
                (make-change-params
                 :set     change-set
                 :exports (join-exports (recursively-export handle)
                                        (change-params-exports inner-change))
                 :syntax  (append (recursively-changed handle)
                                  (change-params-syntax inner-change))))))))
        ((listp syntax)
         (let ((state-syntax
                (utility:foldl-map
                 (lambda (acc syn)
                   (let ((params (recursively-change syn package (car acc))))
                     (list (list (change-params-set params)
                                 (join-exports (change-params-exports params)
                                               (cadr acc)))
                           (change-params-syntax params))))
                 (list change-set +empty-exports+)
                 syntax)))
           (make-change-params :syntax      (cadr state-syntax)
                               :set (caar state-syntax)
                               :exports     (cadar state-syntax))))
        (t (make-change-params :syntax syntax
                               :set change-set))))

(defun recursively-change-symbols (syntax package change-set)
  "This just looks at the symbols in change-set and changes the symbols in the syntax
accordingly"
  (cond ((and (symbolp syntax)
              (utility:curr-packagep syntax)
              (export-set-mem syntax change-set))
         (utility:intern-sym syntax package))
        ((listp syntax)
         (mapcar (lambda (x) (recursively-change-symbols x package change-set)) syntax))
        (t syntax)))

;;;; Predefined handlers------------------------------------------------------------------
(defun cadr-handler (syntax package change-set fn?)
  "handler that changes the cadr, but keeps the cddr the same"
  (declare (ignore change-set))
  (let ((new-cadr (utility:intern-sym-curr-package (cadr syntax) package)))
    (make-handler (list (car syntax) new-cadr)
                  :export (if fn?
                           (make-exports :fn (list new-cadr))
                           (make-exports :var (list new-cadr)))
                  :resume-at (cddr syntax))))

(defun fn-cadr-handler (syntax package change-set)
  (cadr-handler syntax package change-set t))

(defun var-cadr-handler (syntax package change-set)
  (cadr-handler syntax package change-set nil))


(defvar *defun-keywords* '(&key &optional &aux &rest))

(defun defun-handler (syntax package change-set)
  (let* ((new-cadr (utility:intern-sym-curr-package (cadr syntax) package))
         (alias    (alias-handler-gen* (caddr syntax)
                                       package change-set t
                                       *defun-keywords*)))
    (make-handler (list (car syntax)
                        new-cadr
                        (alias-changed alias))
                  :export       (export-fns_ (alias-export alias) (list new-cadr)) 
                  :export-local (alias-export-local alias)
                  :resume-at    (cdddr syntax))))


(defun defclass-handler (syntax package change-set)
  (declare (ignore change-set))
  (let* ((class-name    (utility:intern-sym (cadr syntax) package))
         (super-classes (caddr syntax))
         (slots         (cadddr syntax))
         (options       (cddddr syntax))
         (export        (make-exports :var (list class-name))))
    (labels ((handle-slot-options (options)
               (mapcan (lambda (key-default)
                         (if (member (car key-default)
                                     (list :accessor :reader :writer)
                                     :test #'eq)
                             (let ((new-accessor (utility:intern-sym (cadr key-default) package)))
                               (setf export (export-var_ export new-accessor))
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
         (locally-export (make-exports :fn (remove-if-not #'utility:curr-packagep (mapcar #'car fns))))
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
                                                  *defun-keywords*))
                  (change-set (exports-into-export-set
                               (alias-export alias-args)
                               (car acc)))
                  (params (recursively-change (cddr syntax)
                                              package
                                              (bindle.set:add-seq
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
           (list change-set '())
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


;; Add the handlers

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

(add-handler 'defparameter
             #'var-cadr-handler)

(add-handler 'defvar
             #'fn-cadr-handler)

(add-handler 'deftype
             #'var-cadr-handler)
