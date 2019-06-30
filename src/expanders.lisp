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
           #:recursively-change-symbols
           #:export-to-list
           #:alias-changed
           #:alias-export
           #:alias-export-local
           #:alias-handler-gen*
           #:make-exports
           #:alias-handler-gen*
           #:export-fn_
           #:exports-into-export-set
           #:+empty-exports+
           #:alias-handler*
           #:alias-handler
           #:export-set-mem-fn
           #:export-set-mem-var
           #:export-if-mem-var
           #:export-if-mem-fn
           #:exports-var
           #:exports-fn
           #:exports-typ
           #:exports-mcro))


(in-package #:expanders)

;;;; Types--------------------------------------------------------------------------------
(defstruct exports
  "Handles lists of exports, has functions and variables, so one doesn't accidently
change a function when they want to change a variable and vise versa"
  (fn   bindle.diff-list:+empty+ :type bindle.diff-list:diff-list)
  (var  bindle.diff-list:+empty+ :type bindle.diff-list:diff-list)
  (typ  bindle.diff-list:+empty+ :type bindle.diff-list:diff-list)
  (mcro bindle.diff-list:+empty+ :type bindle.diff-list:diff-list))

(defun export-to-d-list (exports)
  (bindle.diff-list:d-append (exports-fn  exports)
                             (exports-var exports)))

(defun export-to-list (exports)
  (bindle.diff-list:to-list (export-to-d-list exports)))

(defstruct export-set
  "Contains two sets, one set that has change functions, and another set that has chagned
variables"
  (fn   bindle.set:+empty+ :type bindle.set:fset)
  (var  bindle.set:+empty+ :type bindle.set:fset)
  (typ  bindle.set:+empty+ :type bindle.set:fset)
  (mcro bindle.set:+empty+ :type bindle.set:fset))

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
  (changed '()             :type list)
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

(defun curr-package-or-packagep (sym &optional package)
  (or (utility:curr-packagep sym)
      (and package (eq (symbol-package sym) (find-package package)))))

(defun export-var_ (exp var &optional package)
  "A lens to add vars, if the var is in the curr-packagep"
  (if (curr-package-or-packagep var package)
      (make-exports :fn (exports-fn exp)
                    :var (bindle.diff-list:d-cons var (exports-var exp)))
      var))



(defun export-set-var_ (set var &optional package)
  "A lens to add vars, if the var is in the curr-packagep"
  (if (curr-package-or-packagep var package)
      (make-export-set :fn (export-set-fn set)
                       :var (bindle.set:add var (export-set-var set)))
      set))

(defun export-set-fn_ (set fn &optional package)
  (if (curr-package-or-packagep fn package)
      (make-export-set :fn (bindle.set:add fn (export-set-fn set))
                       :var (export-set-var set))
      set))

(defun exports-into-export-set (exp set)
  (make-export-set
   :fn  (bindle.set:add-seq (bindle.diff-list:to-list (exports-fn exp))
                            (export-set-fn set))
   :var (bindle.set:add-seq (bindle.diff-list:to-list (exports-var exp))
                            (export-set-var set))))

(defun join-exports (e1 e2 &rest es)
  (labels ((f (e1 e2) (make-exports :fn  (bindle.diff-list:d-append (exports-fn e1) (exports-fn e2))
                                    :var (bindle.diff-list:d-append (exports-var e1) (exports-var e2)))))
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

(defun export-set-mem-var (elem set)
  (bindle.set:mem elem (export-set-var set)))

(defun export-set-mem-fn (elem set)
  (bindle.set:mem elem (export-set-fn set)))

(defun export-if-mem-var (elem set package)
  (if (export-set-mem-var elem set)
      (utility:intern-sym-curr-package elem package)
      elem))

(defun export-if-mem-fn (elem set package)
  (if (export-set-mem-fn elem set)
      (utility:intern-sym-curr-package elem package)
      elem))

;;;; Global expander table----------------------------------------------------------------

;; we use equal for the test as we have to convert symbols to strings
(defvar *expander-table*
  (make-hash-table :test #'equal))

;;;; Functions for dealing with the expander table----------------------------------------

(declaim (ftype (function (list &key
                                (:resume-at list)
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
;; Note we can compile this more efficiently based on arguments
;; maybe make a macro that does this for me....
;; for example only the last form is used by let and let*
;; Note2: This is only ever used for arguments, so it's safe to
(defun alias-handler-gen* (syntax package change-set *p &optional ignore)
  (macrolet ((update-utility (symb curr-set changed package)
               `(progn
                  (when (utility:curr-packagep ,symb)
                    (when *p
                      (setf ,curr-set (export-set-var_ ,changed ,symb ,package)))
                    (setf export-local (export-var_ export-local ,symb ,package))))))
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
                  (update-utility binding-pair curr-set curr-set package)
                  (utility:intern-sym-curr-package binding-pair package))
                 ;; we have a form like ((:apple a))
                 ((and (listp binding-pair) (listp (car binding-pair)))
                  (let ((changed (recursively-change (cdr binding-pair)
                                                     package
                                                     curr-set)))
                    (update-utility (cadar binding-pair)
                                    curr-set
                                    (change-params-set changed)
                                    package)
                    (setf exports (join-exports exports
                                                (change-params-exports changed)))
                    (cons (list (caar binding-pair)
                                (utility:intern-sym-curr-package (cadar binding-pair)
                                                                 package))
                          (change-params-syntax changed))))
                 (t
                  (let* ((symb       (car binding-pair))
                         (expression (cadr binding-pair))
                         (changed
                          (recursively-change expression package curr-set)))
                    (update-utility symb
                                    curr-set
                                    (change-params-set changed)
                                    package)
                    (setf exports (join-exports exports
                                                (change-params-exports changed)))
                    (list (utility:intern-sym-curr-package symb package)
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
              (export-set-mem-var syntax change-set))
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
                                  :set     (exports-into-export-set (stop-export handle)
                                                                    change-set)))
             (:recursively
              (let* ((change-set   (exports-into-export-set (recursively-export handle)
                                                            change-set))
                     ;; progn is there so the car isn't considered a function, but instead is as
                     ;; it properly is, a progn
                     (inner-change (recursively-change `(progn ,@(recursively-resume-at handle))
                                                       package
                                                       (exports-into-export-set
                                                        (recursively-export-local handle)
                                                        change-set)))
                     (exports      (join-exports (recursively-export handle)
                                                 (change-params-exports inner-change))))
                (make-change-params
                 :set     (exports-into-export-set exports change-set)
                 :exports exports
                 :syntax  (append (recursively-changed handle)
                                  (cdr (change-params-syntax inner-change)))))))))
        ((consp syntax)
         (let* ((first (if (and (symbolp (car syntax))
                                (utility:curr-packagep (car syntax))
                                (export-set-mem-fn (car syntax) change-set))
                           (utility:intern-sym (car syntax) package)
                           (car syntax)))
                (state-syntax
                 (utility:foldl-map
                  (lambda (acc syn)
                    (let ((params (recursively-change syn package (car acc))))
                      (utility:make-fold
                       :acc
                       (list (change-params-set params)
                             (join-exports (change-params-exports params)
                                           (cadr acc)))
                       :place
                       (change-params-syntax params))))
                  (list change-set +empty-exports+)
                  (if (listp (car syntax))
                      syntax
                      (cdr syntax)))))
           (make-change-params :syntax  (if (listp (car syntax))
                                            (utility:fold-place state-syntax)
                                            (cons first (utility:fold-place state-syntax)))
                               :set     (car (utility:fold-acc state-syntax))
                               :exports (cadr (utility:fold-acc state-syntax)))))
        (t (make-change-params :syntax syntax
                               :set change-set
                               :exports +empty-exports+))))

(defun recursively-change-symbols (syntax package change-set &key fn?)
  "This just looks at the symbols in change-set and changes the symbols in the syntax
accordingly"
  (cond ((and (symbolp syntax)
              (utility:curr-packagep syntax)
              (if fn?
                  (export-set-mem-fn syntax change-set)
                  (export-set-mem-var syntax change-set)))
         (utility:intern-sym syntax package))
        ((consp syntax)
         (cons
          (recursively-change-symbols (car syntax) package change-set :fn? t)
          (mapcar (lambda (x) (recursively-change-symbols x package change-set)) (cdr syntax))))
        (t
         syntax)))
