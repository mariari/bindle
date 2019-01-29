(defpackage #:alias
  (:documentation "Allows easy name aliasing for package name spaces")
  (:use #:cl)
  (:export #:with-alias
           #:let-alias))

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

(defmacro with-alias (new-name module-name &body forms)
  "WITH-ALIAS new-name module-name form*

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


(defmacro let-alias (bindings &body form)
  "LET-ALIAS ({(alias [module-name])}*) form*

This macro calls with-alias recursively for all forms letted, changing
alias.f to module-name:f effectively aliasing the old module name to the
new-name given. It then like let, evaluates the form with these changes
Note: we use new-name.f, as CL will not accept the colon as it'll try to resolve
      the name space before we are able to fix it. This will cause some ambiguity
      between symbols with .'s in them if it happens to be new-name.f for the symbol"
  (let ((butlast (butlast bindings))
        (last    (car (last bindings))))
    (labels ((f (binding body-acc)
               `(with-alias ,(car binding) ,(cadr binding) ,body-acc)))
      (reduce #'f
              butlast
              :initial-value (apply #'f last form)
              :from-end t))))
