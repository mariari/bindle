

(defun module-syntax (stream char1 char2)
  "#m SYMBOL-NAME ({#:PACKAGE}*) FORM
This function is used in the syntax above, it creates the module SYMBOL-NAME
while using the packages in the list before evaluating the form in the package
name space of the given one
iff it wasn't already defined, and returns a value with returning what is read
and the package itself for further introspection!"
  (declare (ignore char1 char2))
  (let* ((package   (symbol-name (read stream t nil t)))
         (deps      (read stream t))
         (*package* (or (find-package package)
                        (if (equal "GENSYM" package)
                            (make-package (gensym) :use deps)
                            (make-package package  :use deps)))))
    (list 'values
          (READ stream t t t)
          *package*)))

(set-dispatch-macro-character #\# #\m #'module-syntax)
