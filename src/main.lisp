(defpackage nougat-web
  (:use :cl :serapeum :alexandria :nougat-web.discourse)
  (:import-from :cl-who
   :with-html-output-to-string
                :htm
   :str)
  (:import-from :nougat-web.config
   :app-config
                :get-config)
  (:import-from :cl-fad
   :merge-pathnames-as-directory)
  (:import-from :myway)
  (:export :start :stop))
(in-package :nougat-web)
(named-readtables:in-readtable :interpol-syntax)

;;
;; Setup
;;

(defun from-markdown-file (id)
  (let ((path (fad:merge-pathnames-as-file *static-directory*
                                           (get-config :markdown-path)
                                           (getf (get-config :md-files) id))))
    (with-output-to-string (s)
      (3bmd:parse-and-print-to-stream path s))))

(defclass nougat (ningle:app)
  ((welcome-text
    :initform (from-markdown-file :welcome)
    :accessor welcome)
   (stylesheets
    :initform '("mini.css" "main.css")
    :accessor stylesheets)
   (header-links
    :initform `((,(getf (get-config :discourse) :url) . "Forum")
                ("https://github.com/tu-phys/nougat-web" . "Source"))
    :accessor header-links)))

(setf (who:html-mode) :html5)
(log:config (get-config :log-level))
(defvar *static-directory* (merge-pathnames-as-directory
                            (get-config :application-root)
                            (get-config :static-path)))
(defvar *app* (make-instance 'nougat))
(defvar *whitelisted* nil)
(defvar +ip-whitelist+ (map 'list #'ppcre:create-scanner
                                 (get-config :whitelist)))
(defconstant +ip-header+ (get-config :ip-header))

(defun rot13 (string)
  (map 'string
       (lambda (char &aux (code (char-code char)))
         (if (alpha-char-p char)
             (if (> (- code (char-code (if (upper-case-p char)
                                           #\A #\a))) 12)
                 (code-char (- code 13))
                 (code-char (+ code 13)))
             char))
       string))

(defconstant +root-cisco+
  (apply 'concat
         (map 'list
              #'(lambda (char)
                  (write-to-string (char-code char) :base 16))
              (rot13 (get-config :root))))
  "Cisco magic `encryption` (lolz) performed on the root URL of the
  page.")

(defun whitelisted? (ip)
  (loop :for regex :in +ip-whitelist+
        :when (ppcre:scan regex ip)
          :return t))

;;
;; Layouts
;;

(defmacro with-who (&rest body)
  `(with-html-output-to-string (output nil :prologue t)
     ,@body))

(defun handle-discourse (call)
  (handler-case
      (funcall call)

    (dex:http-request-not-found (c)
      (declare (ignore c))
      (page-404))
    (dex:http-request-bad-gateway (c)
      (declare (ignore c))
      (discourse-502))
    (dexador.error:http-request-forbidden (c)
      (discourse-403 (format nil "~A"
                             (~> (dexador.error:response-body c)
                                 (json:decode-json-from-string)
                                 (aget :errors)))))
    (error (c)
      (discourse-500 (format nil "~A" c)))))

(defmacro with-handle-discourse (&rest body)
  (with-thunk (body)
    `(handle-discourse ,body)))

(defun first-post (topic)
  (url-for :first-post
           :id (write-to-string (aget topic :id))))

(defmacro base ((&key title nav extra-head stylesheets header-links) &body content)
  `(htm
    (:html
     (:head
      (:meta :charset "utf-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:meta :name "robots" :content "index, follow")
      (:title (str #?"Klausur Nougat - ${,title}"))
      ,@extra-head
      (loop for style in (concatenate 'list ,stylesheets (stylesheets *app*))
            collect
            (htm (:link
                  :type "text/css"      ; TODO central def
                  :rel "stylesheet"
                  :href (concatenate 'string "/css/" style)))))
     (:body
      (:header :class "sticky row"
               (:div :class "col-sm-12 col-md-12 col-lg-10 col-lg-offset-2"
                     (:a :href (url-for :home) :class "logo"
                         (:img :src "/images/logo.svg"))
                     (loop for link in (concatenate 'list ,header-links (header-links *app*))
                           collect (htm (:a :class "button" :href (car link)
                                            (str (cdr link)))))
                     (dolist (topic (get-meta-posts 33))
                       (htm
                        (:a :class "button" :href (first-post (cdr topic))
                            (str (car topic)))))
                     ,@nav)))
     (:div :class "container"
           ,@content))))

(defmacro card ((&key title class (type "fluid") extra-title) &body body)
  `(htm
    (:div :class ,(concatenate 'string "card " type " " class)
          (:div :class "section"
                (:h3 :style "display: inline;" (str ,title))
                (htm ,extra-title))
          (:div :class "section"
                (:p ,@body)))))

(defmacro table ((&key class caption) headers rows)
  (let ((header (gensym))
        (row (gensym))
        (el (gensym)))
    `(htm
      (:table :class ,class :caption ,caption
              ,(when headers
                 `(:thead
                   (:tr
                    (loop for ,header in ,headers
                          collecting
                          (htm (:th (str ,header)))))))
              (:tbody
               (loop for ,row in ,rows
                     collecting
                     (htm
                      (:tr
                       (loop for ,el in ,row
                             do
                                (htm (:td  (str ,el))))))))))))

(defun get-forum-url ()
  (getf (get-config :discourse) :url))

(defmacro button (text &optional (link "#"))
  `(htm
      (:a :class "button"
          :href ,link
          (str ,text))))

(defmacro goto-forum-button (link &optional (text "Im Forum ansehen..."))
  (let ((forum-url (getf (get-config :discourse) :url)))
    `(button ,text #?"${,forum-url}/${,link}")))

;;
;; Routes
;;

(defmacro defroute (name (path &optional param-sym &rest nimble-args) &body body)
  (let ((param (gentemp)))
    `(setf (ningle:route *app* ,path ,@nimble-args :identifier ',name)
           #'(lambda (,(if param-sym param-sym param))
               ,@(when (not param-sym)
                   `((declare (ignore ,param))))
               (setf (lack.response:response-headers ningle:*response*)
                     (append (lack.response:response-headers ningle:*response*)
                             (list :content-type "text/html")))
               (let* ((*whitelisted* (whitelisted?
                                      (gethash +ip-header+
                                               (lack.request:request-headers ningle:*request*)))))
                 ,@body)))))

(defroute :home ("/")
  (with-who
      (base (:title "Home")
        (:div :class "row"
              (:div :class "col-sm-12"
                    (card (:title "Willkommen")
                      (str (welcome *app*)))))
        (:div :class "row"
              (:div :class "col-md-3 col-sm-12 col-md-offset-3"
                    (:a :href (url-for :lab-courses)
                        (card (:title "Antestate" :class "selector")
                          "Gesammelte Fragen der Antestate zu den
                                Physik-Praktika der TUD.")))
              (:div :class "col-md-3 col-sm-12"
                    (:a :href (url-for :exams)
                        (card (:title "Altklausuren" :class "selector")
                       "Alte <b>Klausuren, Skripte, Spicker</b> uvm. aus dem
                                Physikstudium an der TUD.")))))))

(defroute :exams ("/exams")
  (with-handle-discourse
   (destructuring-bind (keys table) (get-exam-subjects-table nil)
     (let ((module-route (get-route-by-name :module)))
       (with-who
           (base (:title "Altklausuren")
             (:div
              :class "row"
              (:div
               :class "col-sm-12"
               (card (:title "Altklausuren"
                      :extra-title (goto-forum-button #?"c/${(getf (get-config :discourse) :exam-category)}"))
                 (:table
                     :class "horizontal striped cat-table cat-table"
                     (:thead (loop for key in keys do
                       (htm
                        (:th (str key)))))
                     (:tbody
                      (loop for row in table
                            do (htm
                                (:tr (loop
                                       for el in row
                                       for i from 0
                                       do (if el
                                              (htm
                                               (:td :class "with-cat"
                                                    :data-label (aget el :category)
                                                    (:a
                                                     :href
                                                     (url-for module-route
                                                              :id (write-to-string (aget el :id)))
                                                     :class #?|tooltip ${(when (= 0 i) "bottom")}|

                                                     :aria-label (aget el :name)
                                                     (str (aget el :slug)))))
                                              (htm (:td
                                                    "&nbsp;"))))))))))))))))))
(defroute :lab-course ("/lab-course/:course" params)
  (with-handle-discourse
      (let ((course (get-full-lab-course (aget params :course))))
        (with-who
            (base (:title #?"Antestate - ${(name course)}"
                   :extra-head
                   ((:script :defer "true" :src "https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/katex.min.js")
                    (:script :defer "true" :src "https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/contrib/auto-render.min.js"
                             :onload "renderMathInElement(document.body, {delimiters: [{left: \"$\", right: \"$\", display: false}, {left: \"$$\", right: \"$$\", display: true}]});")
                    (:link
                     :type "text/css"   ; TODO central def
                     :rel "stylesheet"
                     :href "https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/katex.min.css"))) ; TODO: reduce dublication by using clos for modules too!
              (:div
               :class "row"
               (:div
                :class "col-sm-12"
                (card (:title (name course)
                       :extra-title
                       (goto-forum-button #?"/t/${(id course)}"))
                  (if (lab-course-tests-p course)
                      (htm (:div :class "collapse"
                                 (loop :for test :in (tests course)
                                    :for i :from 1
                                    :do (let ((label #?"collapse-section${i}"))
                                          (htm
                                           (:input :type "radio" :id label :aria-hidden "true" :name "accordeon")
                                           (:label :for label :aria-hidden "true"
                                                   (:b (str #?"${(year test)}: ${(tutor test)}")))
                                           (:div (str (body test))))))))

                      (str (body course)))))))))))

(defroute :lab-courses ("/lab-course")
  (with-handle-discourse
      (destructuring-bind (keys table) (get-lab-course-table nil)
        (let ((module-route (get-route-by-name :lab-course)))
          (with-who
              (base (:title "Antestate") ; TODO: reduce dublication by using clos for modules too!
                (:div
                 :class "row"
                 (:div
                  :class "col-sm-12"
                  (card (:title "Antestate"
                         :extra-title
                         (goto-forum-button #?"c/${(getf (get-config :discourse) :lab-course-category)}"))
                    (:table
                        :class "horizontal striped cat-table"
                        (:thead (loop for key in keys do
                          (htm
                           (:th (str key)))))
                        (:tbody
                         (loop for row in table
                               do (htm
                                   (:tr (loop
                                          for el in row
                                          for i from 0
                                          do (if el
                                                 (with-accessors ((name name)
                                                                  (slug slug)
                                                                  (id id)
                                                                  (cat category))
                                                     el
                                                   (htm
                                                    (:td :class "with-cat"
                                                         :data-label cat
                                                         (:a
                                                          :href (url-for module-route :course (write-to-string id))
                                                          :class #?|tooltip ${(when (= 0 i) "bottom")}|
                                                          :aria-label name
                                                          (str slug)))))
                                                 (htm (:td
                                                       "&nbsp;"))))))))))))))))))
(defroute :first-post ("/first-post/:id" params)
  (abind (id) params
    (with-handle-discourse
        (let* ((topic (get-topic id))
               (title (meta-post (aget topic :title)))
               (id (aget topic :id)))
          (if title
              (let ((body (get-first-post-body id)))
                (with-who
                    (base (:title title)
                      (:div :class "row"
                        (:div :class "col-sm-12"
                              (card (:title title
                                     :extra-title
                                     (htm
                                      (goto-forum-button #?"t/${id}")))
                                (str body)))))))
              (page-404)))
      )))

(defun webvpn-url ()
  (let ((path (lack.request:request-path-info ningle:*request*)))
    #?"https://webvpn.zih.tu-dresden.de/+CSCO+1h${+root-cisco+}++${path}"))

(defroute :module ("/exams/:id" params)
  (abind (id) params
    (with-handle-discourse
        (let ((info (get-category-info id))
              (exams (get-exams id))
              (meta-posts (get-meta-posts id)))
          (abind (name) info
            (with-who
                (base (:title #?"Altklausuren - ${name}")
                  (when (not *whitelisted*)
                    (htm (:span :class "toast"
                                (:center "außerhalb Uninetz → Weiterleitung auf Forum"
                                         (:br)
                                         (:i "Anmeldung Erforderlich")
                                         (:br)
                                         (:i "oder direkt mit "
                                             (:a
                                              :href (webvpn-url)
                                              "WebVPN"))))))
                  (:div :class "row"
                        (:div :class "col-sm-12"
                              (card (:title name
                                     :extra-title
                                     (htm
                                      (goto-forum-button #?"c/${(getf (get-config :discourse) :exam-category)}/${id}")
                                      (dolist (topic meta-posts)
                                        (htm (button (car topic)
                                                 (first-post (cdr topic)))))))
                                (:table
                                    :class "striped hoverable exams"
                                    (:thead
                                     (:th)
                                     (:th "Jahr")
                                     (:th "Dozent")
                                     (:th "Bemerkungen")
                                     (:th "Lösungen"))
                                    (:tbody
                                     (dolist (exam exams)
                                       (with-accessors ((year exam-year)
                                                        (prof exam-prof)
                                                        (notes exam-notes)
                                                        (link exam-download)
                                                        (tags exam-tags)
                                                        (topic-id exam-topic-id)
                                                        (solutions exam-solutions))
                                           exam
                                         (htm
                                          (:tr
                                           (:td
                                            :data-label "Download"
                                            (:a
                                             :href (if *whitelisted*
                                                       link #?"${(get-forum-url)}/t/${topic-id}")
                                             :class "button small download"
                                             :style "font-family: u1f400"
                                             (str "⇩"))
                                            (dolist (tag tags)
                                              (htm
                                               (:mark :class "tag" (str (string-upcase tag))))))
                                           (:td :data-label "Jahr" (str year))
                                           (:td :data-label "Dozent" (str prof))
                                           (:td :data-label "Bemerkungen" (str notes))
                                           (:td
                                            :data-label "Lösungen"
                                            (dolist (sol solutions)
                                              (htm
                                               (:a
                                                :href #?"${(get-forum-url)}/t/${topic-id}/${(first sol)}"
                                                :class "button small download tooltip bottom"
                                                :style "font-family: u1f400"
                                                :aria-label (str (cdr sol))
                                                (str "🧻")))))))))))))))))))))

(defroute :discourse-webhook ("/drop-caches/:token" params :method :post)
  (if (string= (aget params :token) (get-config :cache-token))
      (let* ((body (lack.request:request-body-parameters ningle:*request*))
             (headers (lack.request:request-headers ningle:*request*))
             (event (@ headers "x-discourse-event")))
        (log:info "Webhook event: ~A" event)
        (drop-cache event body)
        "ok")
      (page-403)))

(defun get-route-by-name (name)
  (myway:find-route-by-name (ningle/app::mapper *app*)
                            name))

(defgeneric url-for (route &rest params)
  (:method (name &rest params)
    (myway.route:url-for (get-route-by-name name) params))
  (:method ((route myway:route) &rest params)
    (myway.route:url-for route params)))

;;
;; Error Pages
;;

(defun page-404 ()
  "The default 404 page."
  (setf (lack.response:response-status ningle:*response*) 404)
  "Not found!")

(defun page-403 ()
  "The default 404 page."
  (setf (lack.response:response-status ningle:*response*) 403)
  "Access Denied!")

(defun discourse-502 ()
  "To show in case of a discourse 502."
  (setf (lack.response:response-status ningle:*response*) 502)
  "Forum offline.")

(-> discourse-403 (string) string)
(defun discourse-403 (message)
  "To show in case of a 403 from discourse."
  (setf (lack.response:response-status ningle:*response*) 502)
  #?"Discourse Error: ${message}")

(-> discourse-500 (string) string)
(defun discourse-500 (message)
  "To show in case of a 403 from discourse."
  (setf (lack.response:response-status ningle:*response*) 502)
  #?"Unhandled error when calling discourse: ${message}")

;;
;; Helpers
;;

(defvar *handle*)
(defun start ()
  (setf *handle*
        (apply #'clack:clackup
               (lack:builder
                (:static
                 :path (lambda (path)
                         (if (ppcre:scan
                              "^(?:/images/|/css/|/js/|/robots\\.txt$|/favicon\\.ico$)" path)
                             path
                             nil))
                 :root *static-directory*)
                (:backtrace)
                *app*) (getf (app-config) :clack-config))))

(defun stop ()
  (clack:stop *handle*))




;; TODO: Rate Limiter
