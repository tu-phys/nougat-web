(defpackage :nougat-web.discourse
  (:use :cl :serapeum :alexandria)
  (:import-from :nougat-web.config
   :app-config
                :get-config)
  (:export
   :get-exam-subjects
   :get-exam-subjects-table
   :get-exams
   :get-category-info
   :drop-cache

   :exam
   :exam-year
   :exam-prof
   :exam-notes
   :exam-download
   :exam-tags

   :aget
   :abind))

(in-package :nougat-web.discourse)
(named-readtables:in-readtable :interpol-syntax)

(defvar *cache* (make-hash-table :test 'equal))
(defvar *cache-timeout* (get-config :cache-timeout))
(defvar *config* (get-config :discourse))


;; TODO: genral savety and sanity checking

;;
;; Cache Helpers
;;

(defun drop-cache (&optional event body)
  "Empties the cache."
  (switch (event :test #'(lambda (item clauses)
                           (member item clauses :test #'string=)))
    ('("post_created" "post_edited" "post_destroyed")
      (let* ((post
              (cdr (assoc "post" body :test #'string=)))
             (id
              (cdr
               (assoc "topic_id" post :test #'string=))))
        (remhash (topic-url id) *cache*)

        (handler-case
            (let* ((topic (get-topic id))
                   (cat-id (aget topic :category--id)))
              (remhash (exam-list-url cat-id) *cache*))
          (error (e)
            (declare (ignore e))))))
    ('("topic_created" "topic_destroyed")
      nil)
    (t
     (log:info "Full cache drop.")
     (setf *cache* (make-hash-table :test 'equal))))
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
       (if (and ,cached (< (get-universal-time) (+ *cache-timeout* (car ,cached))))
           (progn
             (log:debug "Serving cached: ~A" ,key)
             (cdr ,cached))
           (progn
             (log:debug "Serving uncached: ~A" ,key)
             (cdr
              (setf (gethash ,key *cache*)
                    (cons
                     (get-universal-time)
                     (progn ,@body)))))))))

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
;;
;; API
;;

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

(defgeneric get-category-info (id)
  (:method ((id fixnum))
    (get-category-info (write-to-string id)))
  (:method ((id string))
    (get-cached (#?"/c/${id}/show.json" res)
      (get-category-info (aget res :category))))
  (:method (data)
    (let* ((desc (aget data :description--text))
           (extra-info (parse-category-desc desc)))
      (let ((cat (@ extra-info "Bereich"))) ; TODO: put into config
        (when cat
          (push (cons :category cat) data)))
      data))
  (:documentation "Gets detailed info of a category with the data."))

(defun get-exam-subjects ()
  "Fetches the Modules which have exams available."
  (with-cache :exam-subjects
    (let* ((categories
             (get-cached (#?"/categories.json?parent_category_id=${(getf *config* :exam-category)}"
                            res :exam-categories)
               (aget res :category--list :categories)))
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

(defun get-exam-subjects-table (padding)
  (with-cache :exam-subjects-table
    (flet ((pad (list n pad-el)
             (nconc list (repeat-sequence (list pad-el) (- n (length list))))))
      (let* ((cats (get-exam-subjects))
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
        (list keys table)))))

(define-condition malformed-exam-error (error)
  ((reason :initarg :reason :reader reason)
   (title :initarg :title :reader reason)
   (body :initarg :body :reader body :initform nil)
   (id :initarg :id :reader :id :initform nil)))

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

(defun get-exam (title id tags subject-id)
  "Gets and parses an exam from the discourse API. The title is parsed
with PARSE-EXAM-TITLE. The first link in the body is taken to be the
download link and the rest is parsed as notes. Returns an EXAM."
  (destructuring-bind (year prof) (parse-exam-title title)
    (let* ((first-post (first (aget (get-topic id) :post--stream :posts)))
           (body (aget first-post :cooked)))
      (multiple-value-bind (begin end link-begin link-end)
          (ppcre:scan "href=\"(.*)\">.*(?:$|<br>|</p>)" body)
        (if begin
            (let ((link (subseq body (first-elt link-begin) (first-elt link-end)))
                  (notes (subseq body end)))
              (make-exam :year year :prof prof :download (concatenate 'string (getf *config* :url) link)
                         :notes notes :tags tags :subject-id (parse-integer subject-id)))
            (error 'malformed-exam-error :title title
                                         :body body
                                         :id id
                                         :reason "Link not found."))))))

(defun exam-list-url (id)
  #?"/c/${(getf *config* :exam-category)}/${id}.json")

(-> get-exams ((or string fixnum)) proper-list)
(defun get-exams (subject-id)
  "Gets and parses all the exams in a category scipping invalid ones."
  (get-cached ((exam-list-url subject-id)
                 res)
    (flet ((ex-year (exam)
             (~> (ppcre:split "/" (exam-year exam))
                 (elt 0)
                 (parse-integer))))

      (let* ((topics (aget res :topic--list :topics))
             (exams (loop :for topic :in topics
                          :for exam = (handler-case (get-exam (aget topic :title) (aget topic :id) (aget topic :tags)
                                                              subject-id)
                                        (malformed-exam-error (error)
                                          (log:debug error (slot-value error 'reason)
                                                     (slot-value error 'title)
                                                     (slot-value error 'id))
                                          nil))
                          :when exam :collecting exam)))
        (sort exams #'> :key #'ex-year)))))
