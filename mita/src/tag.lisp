(defpackage :mita.tag
  (:use :cl)
  (:export :tag
           :tag-id
           :tag-name
           :content-id
           :content-type
           :store-list-tags
           :store-add-tag
           :store-get-tag
           :tag-content-id-list
           :content-tags
           :content-tags-set
           :make-store)
  (:import-from :mita.util.threading
                :->>))
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

(labels ((make-row (content tag-id)
           (list (content-id content)
                 (content-type content)
                 tag-id))
         (row-content-id (row)
           (first row))
         (row-content-type (row)
           (second row))
         (row-tag-id (row)
           (third row)))
  (defun read-rows (store)
    (with-open-store-file (stream store "content-tag.csv"
                                  :if-does-not-exist nil)
      (when (listen stream)
        (cl-csv:read-csv stream))))
  
  (defun content-tags (store content)
    (let ((content-id (content-id content)))
      (let ((tag-ids (->> (read-rows store)
                          (remove-if-not (lambda (row)
                                           (string= content-id
                                                    (row-content-id row))))
                          (mapcar #'row-tag-id))))
        (store-list-tags-in store tag-ids))))

  (defun content-tags-set (store content tag-ids)
    (let ((content-id (content-id content)))
      (let ((rows (append
                   (->> (read-rows store)
                        (remove-if (lambda (row)
                                     (string= content-id
                                              (row-content-id row)))))
                   (->> tag-ids
                        (mapcar (lambda (tag-id)
                                  (make-row content tag-id)))))))
        (with-open-store-file (stream store "content-tag.csv"
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
          (cl-csv:write-csv rows
                            :stream stream
                            :always-quote t)))))

  (defun tag-content-id-list (store tag type)
    (let ((tag-id (tag-id tag)))
      (->> (read-rows store)
           (remove-if-not (lambda (row)
                            (and (string= tag-id (row-tag-id row))
                                 (string= type (row-content-type row)))))
           (mapcar #'row-content-id)))))
