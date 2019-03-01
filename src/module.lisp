(defpackage #:module
  (:documentation
   "provides the defmodule macro that gives module functors along with
anonymous modules, module signatures, and other life improvements to CL
package system")
  (:use #:cl #:error-type #:utility #:expanders)
  (:export #:defmodule))

(in-package module)

;;;; Types--------------------------------------------------------------------------------
(defstruct sig-contents
  "a container that holds declarations of the fields below... going to be under
utilized until type checking occurs. Others present the user with a way of saying
just export these symbols"
  (vals     '() :type list)
  (funs     '() :type list)
  (macros   '() :type list)
  (includes '() :type list)
  (others   '() :type list))

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
    (case (error-type:ok-or-error (mod-term mod-term))
      (:sig     `(defparameter ,name
                   ',(error-type:ok-or-error
                      (parse-sig terms))))
      (:struct   (ignore-errors (make-package name))
                 (let* ((sig-exp (gensym))
                        (diff    (gensym))
                        (sig     nil)
                        (upp-exp
                         (cond ((null (car terms))
                                (parse-struct (cdr terms) name))
                               ((symbolp (car terms))
                                (setf sig (car terms))
                                (parse-struct (cdr terms) name))
                               (t
                                (setf sig (parse-sig (cdar terms)))
                                (parse-struct (cdr terms) name))))
                        (new-syn (car upp-exp))
                        (exports (cadr upp-exp)))
                   `(if ,sig
                        (let* ((,sig-exp (sig-export-list ,sig ',name))
                               (,diff    (set-difference ,sig-exp ',exports)))
                          (when ,diff
                            (error (error-parse-struct ,diff)))
                          (progn
                            ,@new-syn
                            (export ,sig-exp ',name)
                            ,(find-package name)))
                        (progn
                          ,@new-syn
                          ,@(final-struct exports name)))))
      (:functor `'undefined))))

(defmacro defmodule (&body terms)
  `(,@terms))

;;;; Helper Functions---------------------------------------------------------------------

(declaim (ftype (function (t) either-error) mode-term))
(defun mod-term (term)
  (if (listp term)
      (list :ok :functor)
      (let ((str-term (and (symbolp term) (symbol-name term))))
        (cond ((equal str-term "SIG")    (list :ok :sig))
              ((equal str-term "STRUCT") (list :ok :struct))
              (t
               (list :error
                     (format nil
                             "The module name should be a functor, STRUCT, or SIG. NOT ~a"
                             term)))))))


(declaim (ftype (function (symbol symbol) string) update-inner-module-name))
(defun update-inner-module-name (outer-module inner-module)
  (concatenate 'string
               (symbol-name outer-module)
               "."
               (symbol-name inner-module)))


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
                      ((equal sym "FUN")     (push-val sig-contents-funs))
                      ((equal sym "MACRO")   (push-val sig-contents-macros))
                      ((equal sym "INCLUDE") (push-val sig-contents-includes))
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
(defun sig-export-list (sig-contents module)
  (mapcar (lambda (sym) (utility:intern-sym sym module))
          (append (sig-contents-vals     sig-contents)
                  (sig-contents-funs     sig-contents)
                  (sig-contents-macros   sig-contents)
                  (sig-contents-includes sig-contents)
                  (sig-contents-others   sig-contents))))


(declaim (ftype (function (list utility:package-designator) list) parse-struct))
(defun parse-struct (syntax package)
  "Parses the body of a module. Returns ether an error or an okay with struct-contents.
   Also checks SIG for the proper values to export."
  (let* ((pass1
          (utility:foldl-map
           (lambda (change-export syntax)
             (let ((params (expanders:recursively-change
                            syntax
                            package
                            (cadr change-export))))
               ;; use difference lists here later!
               (list (list (expanders:join-exports (expanders:change-params-exports params)
                                                   (car change-export))
                           (expanders:change-params-set params))
                     (expanders:change-params-syntax params))))
           (list '() bindle.set:+empty+)
           syntax))
         (syntax     (cadr  pass1))
         (change-set (cadar pass1))
         (exports    (caar  pass1))
         (pass2 (mapcar (lambda (x)
                          (expanders:recursively-change-symbols x package change-set))
                        syntax)))
    (list pass2 exports)))

(declaim (ftype (function (t utility:package-designator) list) final-struct))
(defun final-struct (exports package)
  (list (list 'export
              (list 'quote exports)
              (list 'find-package (list 'quote package)))
        (list 'find-package (list 'quote package))))

(defun error-parse-struct (x)
  (format nil
          "Please include these symbols in your definition ~@a to staisfy your signature"
          x))
