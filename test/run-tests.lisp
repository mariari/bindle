(in-package :cl-module-functor-test)

(defun run-tests ()
  (run! 'aliasing-test)
  (run! 'module-test))
