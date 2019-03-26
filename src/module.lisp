(defpackage #:module
  (:documentation
   "provides the defmodule macro that gives module functors along with
anonymous modules, module signatures, and other life improvements to CL
package system")
  (:use #:cl #:error-type #:utility #:expanders)
  (:export #:defmodule #:defmodule-named #:*functor-name*))

(in-package module)

;;;; Types--------------------------------------------------------------------------------
(defstruct fn-sigs
  fn
  (args '() :type list))

(defparameter *functor-name* (gensym "FUNCTOR-NAME"))

(defstruct sig-contents
  "a container that holds declarations of the fields below... going to be under
utilized until type checking occurs. Others present the user with a way of saying
just export these symbols"
  (vals     '() :type list)
  (funs     '() :type list)
  (macros   '() :type list)
  (includes '() :type list)
  (others   '() :type list))

(defstruct functor-constraint
  (conts (make-sig-contents)                           :type sig-contents)
  (name  (error "Must give functor constraint a name") :type symbol))

(defun sig-update (f sig-contents)
  (make-sig-contents
   :vals     (funcall f (sig-contents-vals     sig-contents))
   :funs     (funcall f (sig-contents-funs     sig-contents))
   :macros   (funcall f (sig-contents-macros   sig-contents))
   :includes (funcall f (sig-contents-includes sig-contents))
   :others   (funcall f (sig-contents-others   sig-contents))))

(defun sig-on (f sig-contents)
  (funcall f (sig-contents-vals     sig-contents))
  (funcall f (sig-contents-funs     sig-contents))
  (funcall f (sig-contents-macros   sig-contents))
  (funcall f (sig-contents-includes sig-contents))
  (funcall f (sig-contents-others   sig-contents)))

(defun sig-map (f sig-contents)
  (sig-update (lambda (x) (mapcar f x)) sig-contents))

(defun sig-mapc (f sig-contents)
  (sig-on (lambda (x) (mapc f x)) sig-contents)
  nil)

;;;; Main function------------------------------------------------------------------------

(defmacro defmodule-named (name &body terms)
  "acts like defmodule, but does not have any reserved names (strict sig) and does not
allow anonymous signatures"
  (let* ((doc-string (and (stringp (car terms)) (car terms)))
         (mod-term   (if doc-string (cadr terms) (car terms)))
         (terms      (if doc-string (cddr terms) (cdr terms))))
    (ecase (error-type:ok-or-error (mod-term mod-term))
      (:sig     `(defparameter ,name
                   ',(error-type:ok-or-error
                      (parse-sig terms))))
      (:struct   (ignore-errors (make-package name))
                 (parse-struct (car terms) (cdr terms) name))
      (:functor `(setf (symbol-function ',name)
                   (parse-functor nil ,(cons mod-term terms)))))))

(defmacro defmodule (&body terms)
  (if (and (symbolp (car terms))
           (not (member (symbol-name (car terms))
                        '("STRUCT" "SIG" "FUNCTOR")
                        :test #'equal)))
      `(defmodule-named ,(car terms) ,@(cdr terms))
      (let* ((doc-string (and (stringp (car terms)) (car terms)))
             (mod-term   (if doc-string (cadr terms) (car terms)))
             (func-terms (if doc-string (cddr terms) (cdr terms))))
        (ecase (error-type:ok-or-error (mod-term mod-term))
          (:sig     (error-type:ok-or-error (parse-sig terms)))
          (:struct  `(defmodule-named ,(gensym) ,@terms))
          (:functor `(parse-functor nil ,func-terms))))))

;;;; Helper Functions---------------------------------------------------------------------

(declaim (ftype (function (t) either-error) mode-term))
(defun mod-term (term)
  (if (listp term)
      (list :ok :functor)
      (let ((str-term (and (symbolp term) (symbol-name term))))
        (cond ((equal str-term "SIG")    (list :ok :sig))
              ((equal str-term "STRUCT") (list :ok :struct))
              ((equal str-term "FUNCTOR") (list :ok :functor))
              (t
               (list :error
                     (format nil
                             "The module name should be a functor, STRUCT, or SIG. NOT ~a"
                             term)))))))


(declaim (ftype (function (symbol symbol) string) update-inner-module-name))
(setf (symbol-function 'update-inner-module-name) #'utility:concat-symbol)


(declaim (ftype (function (list) either-error) parse-sig))
(defun parse-sig (xs)
  "parses the signature of a module functor, and returns either an error
or an okay with the sig-contents"
  (let ((sig-conts (make-sig-contents)))
    (mapc (lambda (x)
            ;; we symbol-name it, as we get namespace issues if not
            ;; ie if we try to call this outside, it'll see macro as
            ;; common-lisp::module, which breaks the case macro
            (if (not (listp x))
                (push x (sig-contents-others sig-conts))
                (macrolet ((push-val (f)
                             `(push (cadr x) (,f sig-conts))))
                  (let ((sym (symbol-name (car x))))
                    (cond
                      ((equal sym "VAL")     (push-val sig-contents-vals))
                      ((equal sym "MACRO")   (push-val sig-contents-macros))
                      ((equal sym "INCLUDE") (push-val sig-contents-includes))
                      ((equal sym "FUN")     (push (make-fn-sigs :fn (cadr x) :args (cddr x))
                                                   (sig-contents-funs sig-conts)))
                      (t (return-from parse-sig
                           (list :error
                                 (concatenate 'string
                                              "the module signature includes a "
                                              sym
                                              " please change it to val, macro, fun or include")))))))))
          xs)
    (list :ok sig-conts)))


;; unused
;; (declaim (ftype (function (sig-contents utility:package-designator) nil)
;;                 sig-export))
(defun sig-export (sig-contents module)
  (sig-mapc (lambda (sym) (utility:intern-sym sym module)) sig-contents))

(declaim (ftype (function (sig-contents utility:package-designator) list)
                sig-export-list))
(defun sig-list (sig-contents)
  (append (sig-contents-vals     sig-contents)
          (sig-contents-macros   sig-contents)
          (sig-contents-includes sig-contents)
          (sig-contents-others   sig-contents)
          (mapcar #'fn-sigs-fn (sig-contents-funs sig-contents))))

(defun sig-export-list (sig-contents module)
  (mapcar (lambda (sym) (utility:intern-sym sym module))
          (sig-list sig-contents)))


(declaim (ftype (function ((or symbol list sig-contents) list utility:package-designator) list)
                parse-struct))
(defun parse-struct (sig syntax package)
  "Parses the body of a module. Returns ether an error or an okay with struct-contents.
   Also checks SIG for the proper values to export."
  (let* ((sig
          (cond ((or (null sig) (eq '() sig)) nil)
                ((symbolp sig)                (symbol-value sig))
                ((sig-contents-p sig)         sig)
                (t                            (parse-sig (cdr sig)))))
         (pass1
          (utility:foldl-map
           (lambda (change-export syntax)
             (let ((params (expanders:recursively-change
                            syntax
                            package
                            (cadr change-export))))
               (list (list (expanders:join-exports (expanders:change-params-exports params)
                                                   (car change-export))
                           (expanders:change-params-set params))
                     (expanders:change-params-syntax params))))
           (list expanders::+empty-exports+ expanders::+empty-export-set+)
           syntax))
         (syntax     (cadr  pass1))
         (change-set (cadar pass1))
         (exports    (expanders:export-to-list (caar pass1)))
         (pass2 (mapcar (lambda (x)
                          (expanders:recursively-change-symbols x package change-set))
                        syntax)))

    (if sig
        (let* ((sig-exp (sig-export-list sig package))
               (diff    (set-difference sig-exp exports)))
          (progn
            `(when ,diff
               (error (error-parse-struct ,diff)))
            `(progn
               ,@pass2
               (export ',sig-exp ',package)
               (values ,(find-package package)
                       ',sig-exp))))
        `(progn
           ,@pass2
           ,@(final-struct exports package)))))

(declaim (ftype (function (t utility:package-designator) list) final-struct))
(defun final-struct (exports package)
  (list (list 'export
              (list 'quote exports)
              (list 'find-package (list 'quote package)))
        (list 'values
              (list 'find-package (list 'quote package))
              (list 'quote exports))))

(defun error-parse-struct (x)
  (format nil
          "Please include these symbols in your definition ~@a to staisfy your signature"
          x))

(defmacro parse-functor (sym syntax)
  (let* ((constraints (car  syntax))
         (sig         (cadr syntax))
         (sig         (cond ((consp sig)
                             (error-type:ok-or-error (parse-sig (cdr sig))))
                            ((listp sig)
                             sig)
                            ((symbolp sig)
                             (symbol-value sig))))
         (body        (cddr syntax))
         (functors    (mapcar (lambda (constraint)
                                (let ((name (car constraint))
                                      (sig  (cadr constraint)))
                                  (make-functor-constraint
                                   :name  (if sym (concat-symbol sym name) name)
                                   :conts (if (listp sig)
                                              (error-type:ok-or-error (parse-sig (cdr sig)))
                                              (symbol-value sig)))))
                              constraints))
         (name        '*functor-name*)
         (struct-name (gensym "Struct-name"))
         (mod         (gensym "MOD"))
         (exps        (gensym "EXPS"))
         (args        (cons name (mapcar (lambda (x) (gensym (symbol-name (car x))))
                                         (car syntax)))))
    `(lambda ,args
       (declare (ignorable ,@(cdr args)))
       ,(reduce (lambda (arg-functor syn)
                  `(alias-signature ,(functor-constraint-name (cadr arg-functor))
                                    ,(car arg-functor)
                                    ,(functor-constraint-conts (cadr arg-functor))
                                    ,syn))
                (mapcar #'list (cdr args) functors)
                :initial-value
                `(multiple-value-bind (,mod ,exps) (defmodule ,struct-name struct ,sig ,@body)
                   ,mod
                   ,(let ((value (if sym `(concat-symbol ',sym ,name) name)))
                      `(values
                        (progn
                          (ignore-errors (delete-package ,value))
                          (make-package ,value :use '(,struct-name)))
                        (prog1
                            ,exps
                          (export ,exps ,value)))))
                :from-end t))))

(defmacro alias-signature (prefix namespace sig body)
  "Aliases all the variables in a signature according to a given namespace "
  (let* ((sig (if (symbolp sig) (symbol-value sig) sig))
         (syn
          `(let-alias ,prefix
               ,namespace
               ,(append (mapcar (lambda (x) (cons (fn-sigs-fn x) (fn-sigs-args x)))
                                (sig-contents-funs sig))
                        (sig-contents-others sig))
             (let-alias-m ,prefix ,namespace ,(sig-contents-macros sig)
               (let-alias-v ,prefix ,namespace ,(sig-contents-vals sig)
                 ,body)))))
    (reduce (lambda (new-sig body)
              `(alias-signature ,prefix ,namespace ,new-sig ,body))
            (sig-contents-includes sig)
            :from-end t
            :initial-value syn)))
;; Load Forms-----------------------------------------------------------------------------
(defmethod make-load-form ((s sig-contents) &optional environment)
  (declare (ignore environment))
  `(module::make-sig-contents :funs     ,(cons 'list (mapcar #'make-load-form (sig-contents-funs s)))
                              :vals     ',(sig-contents-vals s)
                              :others   ',(sig-contents-others s)
                              :macros   ',(sig-contents-macros s)
                              :includes ',(sig-contents-includes s)))

(defmethod make-load-form ((f fn-sigs) &optional environment)
  (declare (ignore environment))
  `(module::make-fn-sigs :fn  (quote ,(fn-sigs-fn f))
                         :args (quote ,(fn-sigs-args f))))
