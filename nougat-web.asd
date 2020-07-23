(defsystem "nougat-web"
  :version "0.1.0"
  :author "Valentin Boettcher"
  :license "LLGPL"
  :depends-on ("ningle" "clack" "cl-who" "log4cl" "envy" "cl-fad" "3bmd" "dexador"
                        "cl-interpol" "cl-json" "cl-arrows" "serapeum" "cl-yaml" "myway"
                        "str")
  :components ((:module "src"
                :components
                ((:file "config")
                 (:file "discourse"
                  :depends-on ("config"))
                 (:file "main"
                  :depends-on ("config" "discourse")))))
  :description ""
  :in-order-to ((test-op (test-op "nougat-web/tests"))))

(defsystem "nougat-web/tests"
  :author "Valentin Boettcher"
  :license "LLGPL"
  :depends-on ("nougat-web"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for nougat-web"
  :perform (test-op (op c) (symbol-call :rove :run c)))
