(in-package :cl-module-functor-test)

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
            (i2 (gen-string)))
    (let ((sym-v  (intern v))
          (sym-f  (intern f))
          (sym-m  (intern m))
          (sym-i1 (intern i1))
          (sym-i2 (intern i2)))
      (is
       (equalp (module::parse-sig `((val     ,sym-v)
                                   (fun     ,sym-f)
                                   (macro   ,sym-m)
                                   (include ,sym-i1)
                                   (include ,sym-i2)))
        (list :ok
              (module::make-sig-contents :vals (list sym-v)
                                         :funs (list sym-f)
                                         :macros (list sym-m)
                                         :includes (list sym-i1 sym-i2)))))

      ;; test the failure case with the user putting macros instead of macro
      (is
       (equalp (module::parse-sig `((val     ,sym-v)
                                   (fun     ,sym-f)
                                   (macros   ,sym-m)
                                   (include ,sym-i1)
                                   (include ,sym-i2)))
        (list :ERROR
              "the module signature includes a MACROS please change it to val, macro, fun or include"))))))
