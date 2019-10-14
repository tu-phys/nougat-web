(defpackage :nougat-web.config
  (:use :cl
   :envy)
  (:export :app-config))
(in-package :nougat-web.config)

(setf (config-env-var) "APP_ENV")

(defconfig :common
    `(:application-root ,(asdf:component-pathname (asdf:find-system :nougat-web))
      :clack-config (:port 8080 :server :woo)
      :static-path "static/"
      :markdown-path "md/"
      :cache-token "test"
      :md-files (:welcome "welcome.md")
      :discourse (:url "https://physik.protagon.space"
                  :key "cd55b5a3b1ef98ba9baf3a57daaa40872e48f7a3ee769b497882ac4e39caf0f6"
                  :username "hiro98"

                  :exam-category 33
                  :lab-course-category 35)))

(defconfig |development|
    '(:log-level :debug
      :cache-timeout 20))

(defconfig |production|
    `(:log-level :info
      :clack-config (:port 8081 :server woo)
      :cache-timeout ,sb-ext:double-float-positive-infinity))

(defun app-config ()
  (envy:config #.(package-name *package*)))

(defun get-config (key)
  (getf (envy:config #.(package-name *package*)) key))
