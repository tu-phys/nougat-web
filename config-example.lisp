(in-package :nougat-web.config)
(named-readtables:in-readtable :interpol-syntax)
(setf *user-config*
      `(:clack-config (:port 8081 :server :woo)
                      :static-path "static/"
                      :markdown-path "md/"
                      :cache-token "test"
                      :md-files (:welcome "welcome.md")
                      :prefix "[KS]"
                      :whitelist (#?|127\.0\.0\.1|)
                      :ip-header "x-real-ip"
                      :max-retry-extra-delay 1
                      :discourse (:url "URL"
                                       :key "key"
                                       :username "username"
                                       :exam-category 33
                                       :lab-course-category 35)))
