(defpackage :nougat-web.config
            (:use :cl
                  :envy)
            (:export :app-config :*user-config*))
(in-package :nougat-web.config)
(named-readtables:in-readtable :interpol-syntax)

(defparameter *user-config* '())
(setf (config-env-var) "APP_ENV")


(defconfig :common
           `(:application-root ,(asdf:component-pathname (asdf:find-system :nougat-web))
                               :clack-config (:port 8081 :server :woo)
                               :static-path "static/"
                               :markdown-path "md/"
                               :cache-token "test"
                               :md-files (:welcome "welcome.md")
                               :prefix "[KS]"
                               :whitelist (#?|127\.0\.0\.1| #?|141\.30\.[0-9]{1,3}\.[0-9]{1,3}|)
                               :ip-header "x-real-ip"
                               :root "https://klausurnoug.at"
                               :max-retry-extra-delay 1
                               :cache-timeout ,sb-ext:double-float-positive-infinity
                               :discourse (:url ""
                                                :key ""
                                                :username "hiro98"
                                                :exam-category 33
                                                :lab-course-category 35
                                                )))


(defconfig |development|
           '(:log-level :debug
                        :cache-timeout 20))

(defconfig |production|
           `(:log-level :info
                        :cache-timeout ,sb-ext:double-float-positive-infinity))

(defun app-config ()
  `(,@*user-config* ,@(envy:config #.(package-name *package*))))

(defun get-config (key)
  (getf (app-config) key))
