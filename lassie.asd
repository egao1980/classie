(defsystem "lassie"
  :version "0.1.0"
  :author "Nikolai Matiushev"
  :license "MIT"
  :depends-on ("alexandria"
               "cl-colors"
               "cl-fad"
               "dufy"
               "lass"
               "parse-float"
               "physical-quantities")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "asdf"))))
  :description "Lassie is the best friend of LASS - utility and functions for translating SCSS to LASS"
  :in-order-to ((test-op (test-op "lassie/tests"))))

(defsystem "lassie/tests"
  :author "Nikolai Matiushev"
  :license "MIT"
  :depends-on ("lassie"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for lassie"
  :perform (test-op (op c) (symbol-call :rove :run c)))
