(asdf:defsystem :bindle
  :version "0.0.0"
  :description "Provides module functors to CL and ease of use of first class modules"
  :author "Mariari and Jacob Rosales Chase"
  :license "MIT"
  :pathname "src/"
  :components
  ((:file "error-type")
   (:file "utility")
   (:file "aliasing")
   (:file "set")
   (:file "diff-list")
   (:file "expanders" :depends-on ("utility" "set" "diff-list"))
   (:file "module" :depends-on ("error-type" "expanders")))
  :in-order-to ((asdf:test-op (asdf:test-op :bindle/test))))

(asdf:defsystem :bindle/test
  :depends-on (:bindle :fiveam)
  :description "testing inferior-shell"
  :pathname "test/"
  :components ((:file "testpkg")
               (:file "module"    :depends-on ("testpkg"))
               (:file "aliasing"  :depends-on ("testpkg"))
               (:file "expanders" :depends-on ("testpkg"))
               (:file "set"       :depends-on ("testpkg"))
               (:file "diff-list" :depends-on ("testpkg"))
               (:file "run-tests" :depends-on ("module" "aliasing" "expanders")))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :bindle-test :run-tests)))
