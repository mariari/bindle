(defpackage #:module
  (:documentation
   "provides the defmodule macro that gives module functors along with
anonymous modules, module signatures, and other life improvements to CL
package system")
  (:use #:cl #:error-type)
  (:export #:defmodule))

(in-package module)

;;;; Types--------------------------------------------------------------------------------
(defstruct sig-contents
  "a container that holds declarations of the fields below... going to be under
utilized until type checking occurs"
  (vals    '() :type list)
  (funs    '() :type list)
  (macros  '() :type list)
  (includes '() :type list))

;;;; Main function------------------------------------------------------------------------
(defmacro defmodule (&body terms)
  `(,@terms))

;;;; Helper Functions---------------------------------------------------------------------

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
  (block error-level
    (list :ok
          (reduce
           (lambda (x acc)
             ;; we symbol-name it, as we get namespace issues if not
             ;; ie if we try to call this outside, it'll see macro as
             ;; common-lisp::module, which breaks the case macro
             (macrolet ((push-val (f)
                          `(progn (push (cadr x) (,f acc)) acc)))
               (let ((sym (symbol-name (car x))))
                 (cond ((equal sym "VAL")     (push-val sig-contents-vals))
                       ((equal sym "FUN")     (push-val sig-contents-funs))
                       ((equal sym "MACRO")   (push-val sig-contents-macros))
                       ((equal sym "INCLUDE") (push-val sig-contents-includes))
                       (t (progn
                            (return-from error-level
                              (list :error
                                    (concatenate 'string
                                                 "the module signature includes a "
                                                 (symbol-name (car x))
                                                 " please change it to val, macro, fun or include")))))))))
           xs
           :from-end t
           :initial-value (make-sig-contents)))))
