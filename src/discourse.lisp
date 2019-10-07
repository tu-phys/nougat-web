(defpackage :nougat-web.discourse
  (:use :cl :serapeum :alexandria)
  (:nicknames :dc)
  (:import-from :nougat-web.config
   :app-config
                :get-config)
  (:export
   :get-exam-subjects
   :get-exam-subjects-table
   :get-exams
   :get-category-info

   :get-lab-courses
   :get-lab-course-table

   :drop-cache

   :exam
   :exam-year
   :exam-prof
   :exam-notes
   :exam-download
   :exam-tags

   :lab-course-rump
   :name
   :slug
   :category

   :aget
   :abind
   :id
   :cat-id))

(in-package :nougat-web.discourse)
(named-readtables:in-readtable :interpol-syntax)

(defvar *cache* (make-hash-table :test 'equal))
(defvar *cache-timeout* (get-config :cache-timeout))
(defvar *config* (get-config :discourse))
(defvar *no-cache* nil)

;; TODO: genral savety and sanity checking

;;
;; Cache Helpers
;;

(defmacro %aget (list test &rest keys)
  (if (car keys)
      `(cdr (assoc ,(car keys) (%aget ,list ,test ,@(cdr keys)) :test ,test))
      list))

(defmacro agett (list test &rest keys)
  `(%aget ,list ,test ,@(nreverse keys)))

(defmacro aget (list &rest keys)
  `(%aget ,list 'eq ,@(nreverse keys)))

(defmacro agets (list &rest keys)
  `(%aget ,list #'string= ,@(nreverse keys)))

(defmacro abind ((&rest keys) list &body body)
  (let ((list-sym (gensym)))
    `(let ((,list-sym ,list))
      (let (,@(loop for key in keys collecting `(,key (aget ,list-sym ,(alexandria:make-keyword key)))))
        ,@body))))

(defun drop-cache (&optional event body)
  "Empties the cache."
  (let ((*no-cache* t))
    (switch (event :test #'(lambda (item clauses)
                             (member item clauses :test #'string=)))
      ('("post_created" "post_edited" "post_destroyed")
        (let* ((post (agets body "post"))
               (id (agets post "topic_id")))
          (handler-case
              (let* ((topic (let ((*no-cache* nil)) (get-topic id)))
                     (cat-id (aget topic :category--id))
                     (cat (get-category-info cat-id))
                     (parent-id (aget cat :parent--category--id)))
                (when parent-id
                  (switch (parent-id)
                    ((getf (get-config :discourse) :exam-category)
                     (get-topic id)
                     (remhash (exam-list-url cat-id) *cache*)
                     (let ((*no-cache* nil)) (get-exams cat-id)))
                    ((getf (get-config :discourse) :lab-course-category)
                     (get-lab-course-table nil)))))
            (error (e)
              (declare (ignore e))))))
      ('("category_created" "category_destroyed" "category_updated")
        (let* ((category (agets body "category"))
               (id (agets category "id"))
               (parent-id (agets category "parent_category_id")))
          (when parent-id
            (switch (parent-id)
              ((getf (get-config :discourse) :exam-category)
               (get-category-info id)
               (get-exam-subjects)
               (when (string= event "category_created")
                 (remhash (exam-list-url id) *cache*)
                 (get-exams id)))
              ((getf (get-config :discourse) :lab-course-category)
               (get-lab-course-table nil))))
          (when (and parent-id
                   (= parent-id (getf (get-config :discourse) :exam-category)))
            )))
      ('("full_drop")                       ; park that option for now
        (log:info "Full cache drop.")
        (setf *cache* (make-hash-table :test 'equal)))
      (t
       nil)))
  *cache*)

(defmacro get-cached ((path result-var &optional key) &body body)
  "Loads a parsed result from the cache if possible or caches the
result of BODY."
  (let ((pathsym (gensym))
        (stream (gensym)))
    `(let* ((,pathsym ,path))
       (with-cache ,(if key key pathsym)
         (with-input-from-string
             (,stream
              (dex:get (concatenate 'string (getf *config* :url) ,pathsym)
                       :headers (list (cons "Api-Key" (getf *config* :key))
                                      (cons "Api-Username" (getf *config* :username)))))
           (let ((,result-var (json:decode-json ,stream))) ,@body))))))

(defmacro with-cache (key &body body)
  "Loads a parsed result from the cache if possible or caches the
result of BODY."
  (let ((cached (gensym)))
    `(let ((,cached (gethash ,key *cache*)))
       (if (and (not *no-cache*) ,cached (< (get-universal-time) (+ *cache-timeout* (car ,cached))))
           (progn
             (log:debug "Loading cached: ~A" ,key)
             (cdr ,cached))
           (progn
             (log:debug "Loading uncached: ~A" ,key)
             (cdr
              (setf (gethash ,key *cache*)
                    (cons
                     (get-universal-time)
                     (progn ,@body)))))))))

;;
;; API
;;

(deftype category-type () '(member :exam :lab-course))
(defvar *meta-sep-scanner* (ppcre:create-scanner "---+")) ; TODO: put in config
(defun parse-meta-block (desc)                            ; TODO: exceptions...
  "Parses a yaml formatted meta block separated by *META-SEP-SCANNER* into a hash table.
This hash table may be empty."
  (let ((meta (second (multiple-value-list (ppcre:scan *meta-sep-scanner* desc)))))
    (if meta
        (yaml:parse (subseq desc 0 meta))
        (make-hash-table :test #'equalp))))

(defun parse-category-desc (desc)
  "Parse the one-paragraph category description.
Takes DESC like ~key: value; key1: value and returns a dictionary."
  (let ((parsed (make-hash-table :test 'equal)))
    (if (and (stringp desc) (string= (char desc 0) #\~)) ; TODO: error handling
        (let ((values (ppcre:split ";" (subseq desc 1))))
          (dolist (entry values)
            (let ((split (ppcre:split ":\s*" entry)))
              (when (= (length split) 2)
                (setf (@ parsed (trim-whitespace (first split))) (trim-whitespace (second split))))))
          parsed)
        parsed)))

(defun category-details-url (id)
  #?"/c/${id}/show.json")

(defgeneric get-category-info (id)
  (:method ((id fixnum))
    (get-category-info (write-to-string id)))
  (:method ((id string))
    (get-cached ((category-details-url id) res)
      (get-category-info (aget res :category))))
  (:method (data)
    (let* ((desc (aget data :description--text))
           (extra-info (parse-category-desc desc)))
      (let ((cat (@ extra-info "Bereich"))) ; TODO: put into config
        (when cat
          (push (cons :category cat) data)))
      data))
  (:documentation "Gets detailed info of a category with the data."))

(defun get-subcategories (id)
  "Gets a list of subcategories of the category with ID."
  (get-cached
      (#?"/categories.json?parent_category_id=${id}" res)
    (aget res :category--list :categories)))

(defun get-exam-subjects ()
  "Fetches the Modules which have exams available."
  (with-cache :exam-subjects
    (let* ((categories
             (get-subcategories (getf *config* :exam-category)))
           (subjects (loop for cat-data in categories
                           for cat = (get-category-info cat-data)
                           when cat collecting cat))
           (sorted (assort subjects :key #'(lambda (el) (aget el :category)) :test #'string=))
           (sorted-hash (make-hash-table :test 'equal)))
      (dolist (cat sorted)
        (let ((cat-id (aget (first cat) :category)))
          (when cat-id
            (setf (@ sorted-hash cat-id)
                  cat))))
      sorted-hash)))

(defun hash->padded-table (hash padding)
  "Sorts a hash table HASH into a list of headers and rows akin to a html
table. Padds missing elements with TABLE."
  (flet ((pad (list n pad-el)
           (nconc list (repeat-sequence (list pad-el) (- n (length list))))))
    (let* ((cats hash)
           (keys (hash-table-keys cats))
           (values (mapcar #'(lambda (el)
                               (@ cats el))
                           keys))
           (maxlength (apply #'max (mapcar #'length
                                           values)))
           (padded (mapcar #'(lambda (el)
                               (pad el maxlength padding))
                    values))
           (table (apply #'mapcar #'list padded)))
      (list keys table))))

(defun get-exam-subjects-table (padding)
  (with-cache :exam-subjects-table
    (hash->padded-table (get-exam-subjects) padding)))

(define-condition malformed-topic-error (error)
  ((reason :initarg :reason :reader reason)
   (title :initarg :title :reader reason)
   (body :initarg :body :reader body :initform nil)
   (id :initarg :id :reader :id :initform nil)))

(define-condition malformed-exam-error (malformed-topic-error)
  ())

(define-condition malformed-lab-couse-error (malformed-topic-error)
  ())

(defstruct exam
  (year "" :type string)
  (prof "" :type string)
  (download "" :type string)
  (notes "" :type string)
  (tags nil :type proper-sequence)
  (subject-id "" :type fixnum))

(defun parse-exam-title (title)
  "Parses the exam TITLE looking like `year: professor` into a list of
shape (YEAR TITLE)."
  (multiple-value-bind (matched matches) (ppcre:scan-to-strings "([0-9]+(?:/[0-9]+)?):\\s*(.+)" title)
    (if (and matched (= 2 (length matches)))
        (list (trim-whitespace (elt matches 0)) (trim-whitespace (elt matches 1)))
        (error 'malformed-exam-error :reason "Invalid Title"
                                     :title title))))

(defun topic-url (id)
  #?"/t/${id}.json")

(defun get-topic (id)
  (get-cached ((topic-url id) res)
    res))

(defun get-exam (topic subject-id)
  "Gets and parses an exam from the discourse API. The title is parsed
with PARSE-EXAM-TITLE. The first link in the body is taken to be the
download link and the rest is parsed as notes. Returns an EXAM."
  (abind (title id tags) topic
    (destructuring-bind (year prof) (parse-exam-title title)
      (let* ((first-post (first (aget (get-topic id) :post--stream :posts)))
             (body (aget first-post :cooked)))
        (multiple-value-bind (begin end link-begin link-end)
            (ppcre:scan "href=\"(.*)\">.*(?:$|<br>|</p>)" body)
          (if begin
              (let ((link (subseq body (first-elt link-begin) (first-elt link-end)))
                    (notes (subseq body end)))
                (make-exam :year year :prof prof :download (concatenate 'string (getf *config* :url) link)
                           :notes notes :tags tags :subject-id (if (stringp subject-id)
                                                                   (parse-integer subject-id)
                                                                   subject-id)))
              (error 'malformed-exam-error :title title
                                           :body body
                                           :id id
                                           :reason "Link not found.")))))))

(defun exam-list-url (id)
  #?"/c/${(getf *config* :exam-category)}/${id}.json")

(-> get-exams ((or string fixnum)) proper-list)
(defun get-exams (subject-id)
  "Gets and parses all the exams in a category skipping invalid ones."
  (get-cached ((exam-list-url subject-id)
                 res)
    (flet ((ex-year (exam)
             (~> (ppcre:split "/" (exam-year exam))
                 (elt 0)
                 (parse-integer))))

      (let* ((topics (aget res :topic--list :topics))
             (exams (loop :for topic :in topics
                          :for exam = (handler-case (get-exam topic
                                                              subject-id)
                                        (malformed-exam-error (error)
                                          (log:debug error (slot-value error 'reason)
                                                     (slot-value error 'title)
                                                     (slot-value error 'id))
                                          nil))
                          :when exam :collecting exam)))
        (sort exams #'> :key #'ex-year)))))

;;
;; Lab Courses
;;

(defun lab-list-url (id)
  #?"/c/${(getf *config* :lab-course-category)}/${id}.json")

(eval-always
  (defun make-auto-slot (definiton accessor)
    (destructuring-bind (name type &rest rest) definiton
      (let ((slot `(,name :type ,type :initarg ,(make-keyword name) ,@rest)))
        (if accessor
            (nconc slot `(:accessor ,name))
            slot)))))

(defmacro defautoclass (name superclasses slot-definitions &rest rest)
  `(defclass ,name ,superclasses
     ,@(loop :for (accessor definions) :in slot-definitions
             :collecting (mapcar #'(lambda (def) (make-auto-slot def accessor))
                                 definions))
     ,@rest))

(defautoclass lab-course-rump ()
  ((:reader
    ((id fixnum)
     (name string)
     (slug string)
     (category string)))))

(defautoclass lab-test ()
  ((:reader ((tutor string)
             (year fixnum)
             (questions proper-list)
             (notes string)))))

(defautoclass lab-course (lab-couse-rump)
  ((:reader
    ((tests proper-list)))))

(defun parse-lab-course-topic (topic category)
  (abind (title id) topic
         (multiple-value-bind (match parts) (ppcre:scan-to-strings "(.*?):\\s*(.*)" title)
           (if (or (not match) (< (length parts) 2))
               (error 'malformed-lab-couse-error
                      :id id
                      :title title
                      :reason "Malformed Title")
               (make-instance 'lab-course-rump
                              :category category
                              :id id :name (elt parts 1) :slug (elt parts 0))))))

(defun get-lab-courses ()
  "Retrieves a hash-table of lab-courses keyed by their superior course. "
  (let ((courses (get-subcategories (getf *config* :lab-course-category)))
        (lab-table (make-hash-table :test 'equal)))
    (dolist (lab courses)
      (abind (name id) lab
        (setf (@ lab-table name)
              (get-cached ((lab-list-url id) res) ; TODO: reduce code dublication
                (let ((topics (aget res :topic--list :topics)))
                  (loop :for topic :in topics
                        :for lab-course =
                                        (handler-case (parse-lab-course-topic topic name)
                                          (malformed-topic-error (error)
                                            (log:debug error (slot-value error 'reason)
                                                       (slot-value error 'title)
                                                       (slot-value error 'id))
                                            nil))
                        :when lab-course :collecting lab-course))))))
    lab-table))

(defun get-lab-course-table (padding)
  (with-cache :lab-course-table
   (hash->padded-table (get-lab-courses) padding)))
