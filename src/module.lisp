(defpackage #:module
  (:documentation
   "provides the defmodule macro that gives module functors along with
anonymous modules, module signatures, and other life improvements to CL
package system")
  (:use #:cl)
  (:export))

(in-package module)

(declaim (ftype (function (symbol symbol) string) update-inner-module-name))
(defun update-inner-module-name (outer-module inner-module)
  (concatenate 'string
               (symbol-name outer-module)
               "."
               (symbol-name inner-module)))
