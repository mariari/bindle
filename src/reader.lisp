

(defun module-syntax (stream char1 char2)
  (declare (ignore char1 char2))
  (let* ((package   (symbol-name (read stream t nil nil)))
         (deps      (read stream t))
         (*package* (or (find-package package)
                        (if (equal "GENSYM" package)
                            (make-package (gensym) :use deps)
                            (make-package package  :use deps)))))
    (READ stream t t t)))

(set-dispatch-macro-character #\# #\m #'module-syntax)
