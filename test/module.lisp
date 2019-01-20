(in-package :cl-module-functor-test)

(def-suite module-test
    :description "Tests the module package")

(in-suite module-test)

(test update-inn-ermodule-name
  (for-all ((m (gen-string))
            (n (gen-string)))
    (is
     (equal (module::update-inner-module-name (intern m) (intern n))
            (concatenate 'string m "." n)))))
