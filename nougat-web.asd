(defsystem "nougat-web"
           :version "0.1.0"
           :author "Valentin Boettcher"
           :license "LLGPL"
           :depends-on ("ningle" "lack" "lack-middleware-static" "clack" "cl-who" "log4cl" "envy" "cl-fad" "3bmd" "dexador"
                        "cl-interpol" "cl-json" "arrow-macros" "serapeum" "cl-yaml" "myway"
                        "str" "woo")
           :components ((:module "src"
                                 :components
                                 ((:file "config")
                                  (:file "discourse"
                                         :depends-on ("config"))
                                  (:file "main"
                                         :depends-on ("config" "discourse")))))
           :description "")
