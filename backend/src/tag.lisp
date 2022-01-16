(defpackage :mita.tag
  (:use :cl)
  (:export :tag
           :tag-id
           :tag-name
           :tag-contents
           :store-list-tags
           :store-add-tag
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
                            ,@args)
     ,@body))

(labels ((row->tag (row)
           (make-tag :id (uuid:make-uuid-from-string
                          (first row))
                     :name (second row)))
         (tag->row (tag)
           (list (format nil "~A" (tag-id tag))
                 (tag-name tag))))

  (defun store-list-tags (store)
    (with-open-store-file (stream store "tags.csv"
                                  :if-does-not-exist nil)
      (when (listen stream)
        (cl-csv:read-csv stream :map-fn #'row->tag))))

  (defun store-list-tags-in (store tag-ids)
    (let ((tags (store-list-tags store)))
      (remove-if-not (lambda (tag)
                       (find (tag-id tag) tag-ids :test #'uuid:uuid=))
                     tags)))
                       
  (defun store-add-tag (store name)
    (let ((tag (make-tag :id (uuid:make-v4-uuid)
                         :name name)))
      (with-open-store-file (stream store "tags.csv"
                                    :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
        (cl-csv:write-csv (list (tag->row tag)) :stream stream))
      tag)))

;;;

(labels ((row->content-id (row)
           (first row))
         (row->content-type (row)
           (second row))
         (row->tag-ids (row)
           (mapcar #'uuid:make-uuid-from-string
                   (split-sequence:split-sequence #\, (third row))))
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
                 (row->tag-ids
                  (find-if (lambda (row)
                             (string= (row->content-id row)
                                      content-id))
                           (nreverse (cl-csv:read-csv stream)))))))))
      (store-list-tags-in store tag-ids)))

  (defun content-tags-set (store content tag-ids)
    (with-open-store-file (stream store "content-tag.csv"
                                    :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
      (cl-csv:write-csv (list (->row content tag-ids))
                        :stream stream
                        :always-quote t)))

  (defun tag-contents (store tag type)
    (with-open-store-file (stream store "content-tag.csv"
                                  :if-does-not-exist nil)
      (when (listen stream)
        (let ((content-ids nil)
              (scanned-hash (make-hash-table :test #'equal)))
          (dolist (row (cl-csv:read-csv stream))
            (let ((content-id (row->content-id row)))
              (when (and (not (gethash content-id scanned-hash))
                         (string= (row->content-type row) type))
                (when (find-if (lambda (tag-id)
                                 (uuid:uuid= tag-id (tag-id tag)))
                               (row->tag-ids row))
                  (push content-id content-ids))
                (setf (gethash content-id scanned-hash) t))))
          content-ids)))))

;;;

(defmethod content-id ((c mita.file:folder))
  (mita.file:file-path c))

(defmethod content-type ((c mita.file:folder))
  "folder")

#+nil
(progn
  (defmethod content-id (c)
    "id")
  (defmethod content-type (c)
    "type"))
