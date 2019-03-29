(in-package :bindle-test)

(def-suite module-test
    :description "Tests the module package")

(in-suite module-test)

(test update-inner-module-name
  (for-all ((m (gen-string))
            (n (gen-string)))
    (is
     (equal (module::update-inner-module-name (intern m) (intern n))
            (concatenate 'string m "." n)))))


(test parse-sig
  (for-all ((v  (gen-string))
            (f  (gen-string))
            (m  (gen-string))
            (i1 (gen-string))
            (i2 (gen-string))
            (o1 (gen-string))
            (o2 (gen-string)))
    (let ((sym-v  (intern v))
          (sym-f  (intern f))
          (sym-m  (intern m))
          (sym-i1 (intern i1))
          (sym-i2 (intern i2))
          (sym-o1 (intern o1))
          (sym-o2 (intern o2)))
      (is
       (equalp (module::parse-sig `((val     ,sym-v)
                                    (fun     ,sym-f arg1 arg2)
                                    (macro   ,sym-m)
                                    (include ,sym-i1)
                                    (include ,sym-i2)
                                    ,sym-o1
                                    ,sym-o2))
               (list :ok
                     (module::make-sig-contents :vals     (list sym-v)
                                                :funs     (list (module::make-fn-sigs :fn sym-f
                                                                                      :args '(arg1 arg2)))
                                                :macros   (list sym-m)
                                                :includes (list sym-i2 sym-i1)
                                                :others   (list sym-o2 sym-o1)))))

      ;; test the failure case with the user putting macros instead of macro
      (is
       (equalp
        (module::parse-sig `((val     ,sym-v)
                            (fun     ,sym-f)
                            (macros   ,sym-m)
                            (include ,sym-i1)
                            (include ,sym-i2)))
        (list :ERROR
              "the module signature includes a MACROS please change it to val, macro, fun or include"))))))

(test defmodule
  (is
   (equal
    (macroexpand-1
     (macroexpand-1
      '(module:defmodule fooz struct ()
        (defun test::foo (x) x)
        (+ (test::foo 2) (foo 3)))))
    '(progn
      (defun test::foo (fooz::x) fooz::x)
      (+ (test::foo 2) (foo 3))
      (export 'nil (find-package 'fooz))
      (values (find-package 'fooz) 'nil))))
  (is
   (equal
    (macroexpand-1
     (macroexpand-1
      '(module:defmodule fooz struct ()
        (defclass circle () ())
        (defgeneric blah (:doc "blah"))
        (defmethod blah ((shape circle))
          shape)
        (defmethod booz ((shape square))
          shape))))
    '(progn
      (defclass fooz::circle nil nil)
      (defgeneric fooz::blah
          (:doc "blah"))
      (defmethod fooz::blah ((fooz::shape fooz::circle))
        fooz::shape)
      (defmethod booz ((fooz::shape square))
        fooz::shape)
      (export '(fooz::blah fooz::circle) (find-package 'fooz))
      (values (find-package 'fooz) '(fooz::blah FOOZ::CIRCLE)))))
  (is
   (equal
    (macroexpand-1
     (macroexpand-1
      '(module:defmodule fooz struct ()
        (defun beep () 2)
        (function beep)
        #'beep
        #'boop
        (list boosh beep x *blah* (*blah* 3)))))
    '(progn
      (defun fooz::beep () 2)
      #'fooz::beep
      #'fooz::beep
      #'boop
      (list boosh beep x *blah* (*blah* 3))
      (export '(fooz::beep) (find-package 'fooz))
      (values (find-package 'fooz) '(fooz::beep))))))
