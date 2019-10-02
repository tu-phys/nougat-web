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
                              "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
                             path
                             nil))
                 :root *static-directory*)
                *app*) (getf (app-config) :clack-config))))

(defun stop ()
  (clack:stop *handle*))

(defun from-markdown-file (id)
  (let ((path (fad:merge-pathnames-as-file *static-directory*
                                           (get-config :markdown-path)
                                           (getf (get-config :md-files) id))))
    (with-output-to-string (s)
      (3bmd:parse-and-print-to-stream path s))))


;; TODO: Rate Limiter

;;
;; Setup
;;

(defclass nougat (ningle:app)
  ((welcome-text
    :initform (from-markdown-file :welcome)
    :accessor welcome)
   (stylesheets
    :initform '("mini.css" "main.css")
    :accessor stylesheets)
   (header-links
    :initform `((,(getf (get-config :discourse) :url) . "Forum"))
    :accessor header-links)))

(setf (who:html-mode) :html5)
(log:config (getf (app-config) :log-level))
(defvar *static-directory* (merge-pathnames-as-directory
                            (get-config :application-root)
                            (get-config :static-path)))
(defvar *app* (make-instance 'nougat))

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

(defmacro base ((&key title nav stylesheets header-links) &body content)
  `(htm
    (:html
     (:head
      (:meta :charset "utf-8")
      (:title (str #?"Klausurnougat - ${,title}"))
      (loop for style in (concatenate 'list ,stylesheets (stylesheets *app*))
            collect
            (htm (:link
                  :type "text/css"      ; TODO central def
                  :rel "stylesheet"
                  :href (concatenate 'string "/css/" style)))))
     (:body
      (:header :class "sticky"
               (:a :href (url-for :home) :class "logo"
                   (:img :src "/images/logo.svg")
                   (loop for link in (concatenate 'list ,header-links (header-links *app*))
                         collect (htm (:a :class "button" :href (car link)
                                          (str (cdr link)))))
                   ,@nav)))
     (:div :class "container"
           ,@content))
    (:footer :class "sticky"
             (:a :href "https://github.com/tu-phys/nougat-web" "Source"))))

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

(defmacro goto-forum-button (link &optional (text "Im Forum ansehen..."))
  (let ((forum-url (getf (get-config :discourse) :url)))
    `(htm
      (:a :class "button"
          :href #?"${,forum-url}/${,link}"
          (str ,text)))))
;;
;; Routes
;;

(defmacro defroute (name (path &optional param-sym &rest nimble-args) &body body)
  (let ((param (gentemp)))
    `(setf (ningle:route *app* ,path ,@nimble-args :identifier ',name)
           #'(lambda (,(if param-sym param-sym param))
               ,(when (not param-sym)
                  `(declare (ignore ,param)))
               ,@body))))

(defroute :home ("/")
  (with-who
      (base (:title "Home")
        (:div :class "row"
              (:div :class "col-sm-12"
                    (card (:title "Willkommen")
                      (str (welcome *app*)))))
        (:div :class "row"
              (:div :class "col-md-3 sm-12 col-md-offset-3"
                    (card (:title "Antestate" :class "selector")
                      "Gesammelte Fragen der Antestate zu
                                den Physik-Praktika der TUD. <br> <i> Coming Soon </i>"))
              (:div :class "col-md-3 sm-12"
                    (:a :href (url-for :exams)
                     (card (:title "Altklausuren" :class "selector")
                       "Alte Klausuren aus dem Physik
                                an der TUD.")))))))

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
                                       do (if el
                                              (htm
                                               (:td :class "with-cat"
                                                    :data-label (aget el :category)
                                                    (:a
                                                     :href
                                                     (url-for module-route
                                                              :id (write-to-string (aget el :id)))
                                                     :class "tooltip"
                                                     :aria-label (aget el :name)
                                                     (str (aget el :slug)))))
                                              (htm (:td
                                                    "&nbsp;"))))))))))))))))))

(defroute :module ("/exams/:id" params)
  (abind (id) params
    (with-handle-discourse
        (let ((info (get-category-info id))
              (exams (get-exams id)))
          (abind (name) info
            (with-who
                (base (:title #?"Altklausuren - ${name}")
                  (:div :class "row"
                        (:div :class "col-sm-12"
                              (card (:title name
                                     :extra-title
                                     (goto-forum-button #?"c/${(getf (get-config :discourse) :exam-category)}/${id}"))
                                (:table
                                    :class "striped hoverable exams"
                                    (:thead
                                     (:th)
                                     (:th "Jahr")
                                     (:th "Dozent")
                                     (:th "Bemerkungen"))
                                    (:tbody
                                     (dolist (exam exams)
                                       (with-accessors ((year exam-year)
                                                        (prof exam-prof)
                                                        (notes exam-notes)
                                                        (link exam-download)
                                                        (tags exam-tags))
                                           exam
                                         (htm
                                          (:tr
                                           (:td
                                            :data-label "Download"
                                            (:a
                                             :href link
                                             :class "button small download"
                                             :style "font-family: u1f400"
                                             (str "⇩"))
                                            (dolist (tag tags)
                                              (htm
                                               (:mark :class "tag" (str (string-upcase tag))))))
                                           (:td :data-label "Jahr" (str year))
                                           (:td :data-label "Dozent" (str prof))
                                           (:td :data-label "Bemerkungen" (str notes))))))))))))))))))

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
