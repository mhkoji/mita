(defpackage :mita.tag
  (:use :cl)
  (:export :tag
           :tag-id
           :tag-name
           :tag-content-id-list
           :store-list-tags
           :store-add-tag
           :store-get-tag
           :content-tags
           :content-tags-set
           :make-store))
(in-package :mita.tag)

(defstruct tag id name)
(defgeneric content-id (content))
(defgeneric content-type (content))

;;;

(defstruct store
  dir)

(defmacro with-open-store-file ((stream store file &rest args)
                                &body body)
  `(with-open-file (,stream (merge-pathnames ,file (store-dir ,store))
                            :external-format :utf-8
                            ,@args)
     ,@body))

(labels ((row->tag (row)
           (make-tag :id (first row)
                     :name (second row)))
         (tag->row (tag added-at)
           (list (tag-id tag) (tag-name tag) added-at)))
  (defun store-list-tags (store)
    (with-open-store-file (stream store "tags.csv"
                                  :if-does-not-exist nil)
      (when (listen stream)
        (cl-csv:read-csv stream :map-fn #'row->tag))))

  (defun store-list-tags-in (store tag-ids)
    (let ((tags (store-list-tags store)))
      (remove-if-not (lambda (tag)
                       (find (tag-id tag) tag-ids :test #'string=))
                     tags)))

  (defun store-get-tag (store tag-id)
    (car (store-list-tags-in store (list tag-id))))
                       
  (defun store-add-tag (store name)
    (let ((tag (make-tag
                :id (format nil "~A" (uuid:make-v4-uuid))
                :name name))
          (added-at (local-time:format-rfc3339-timestring
                     nil (local-time:now))))
      (with-open-store-file (stream store "tags.csv"
                                    :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
        (cl-csv:write-csv (list (tag->row tag added-at)) :stream stream))
      tag)))

;;;

(labels ((row->content-id (row)
           (first row))
         (row->content-type (row)
           (second row))
         (row->tag-id (row)
           (third row))
         #+nil
         (->row (content tag-ids)
           (list (content-id content)
                 (content-type content)
                 (format nil "~{~A~^,~}" tag-ids))))
  (defun content-tags (store content)
    (let ((tag-ids
           (with-open-store-file (stream store "content-tag.csv"
                                         :if-does-not-exist nil)
             (when (listen stream)
               (let ((content-id (content-id content)))
                 (mapcar #'row->tag-id
                         (remove-if-not (lambda (row)
                                          (string= (row->content-id row)
                                                   content-id))
                                        (cl-csv:read-csv stream))))))))
      (store-list-tags-in store tag-ids)))

  (defun content-tags-set (store content tag-ids)
    #+nil
    (with-open-store-file (stream store "content-tag.csv"
                                    :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
      (cl-csv:write-csv (list (->row content tag-ids))
                        :stream stream
                        :always-quote t)))

  (defun tag-content-id-list (store tag type)
    (with-open-store-file (stream store "content-tag.csv"
                                  :if-does-not-exist nil)
      (when (listen stream)
        (let ((tag-id (tag-id tag))
              (content-ids nil))
          (dolist (row (cl-csv:read-csv stream))
            (when (and (string= tag-id (row->tag-id row))
                       (string= type (row->content-type row)))
              (push (row->content-id row) content-ids)))
          content-ids)))))

;;;

(defmethod content-id ((c mita.file:folder))
  (namestring (mita.file:file-path c)))

(defmethod content-type ((c mita.file:folder))
  "folder")

#+nil
(progn
  (defmethod content-id (c)
    "id")
  (defmethod content-type (c)
    "type"))
