(defpackage #:alias
  (:documentation "Allows easy name aliasing for package name spaces")
  (:use #:cl)
  (:export #:with-alias))

(in-package alias)

(declaim (ftype (function (symbol string) boolean) alias-p))
(defun alias-p (alias-name string)
  "checks to see if the STRING is alias-name.*"
  (let ((pos (position #\. string)))
    (and pos
         (equalp (symbol-name alias-name)
                 (subseq string 0 pos)))))

(declaim (ftype (function (symbol string) string) alias-function-name))
(defun alias-function-name (alias-name string)
  "removes the alias-name from alias-name.*, note that this will error when
the given string is smaller than alias-name which means a precondition is violated"
  (let ((alias-string (symbol-name alias-name)))
    (subseq string
            (1+ (length alias-string)))))

(defmacro with-alias (module-name new-name &body forms)
  "WITH-ALIAS module-name new-name form*

everywhere inside FORMS new-name.f will be replaced with module-name:f,
effectively aliasing the old module name to the new-name given
Note: we use new-name.f, as CL will not accept the colon as it'll try to resolve
      the name space before we are able to fix it. This will cause some ambiguity
      between symbols with .'s in them if it happens to be new-name.f for the symbol"
  (flet ((alias-p      (alias?) (alias-p             new-name (symbol-name alias?)))
         (alias-f-name (alias)  (alias-function-name new-name (symbol-name alias))))
    (labels ((walk-body (sexp)
               (cond
                 ((consp sexp)         (mapcar #'walk-body sexp))
                 ((not (symbolp sexp)) sexp)
                 ((alias-p sexp)       (intern (alias-f-name sexp) (find-package module-name)))
                 (t                    sexp))))
      `(progn ,@(mapcar #'walk-body forms)))))


(defmacro let-alias () `())
