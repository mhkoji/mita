(defpackage :mita.postgres.db
  (:use :cl)
  (:import-from :alexandria
                :when-let)
  (:export :postgres
           :with-transaction))
(in-package :mita.postgres.db)

(defclass postgres (mita.db:db)
  ((conn
    :initarg :conn
    :reader postgres-conn)))

(defmacro with-transaction ((db &key database user host port) &body body)
  (let ((g-conn (gensym)))
    `(let ((,g-conn (postmodern:connect ,database ,user "" ,host
                                        :port ,port)))
       (unwind-protect
            (let ((postmodern:*database* ,g-conn))
              (postmodern:with-transaction (nil :serializable)
                (let ((,db (make-instance 'postgres :conn ,g-conn)))
                  ,@body)))
         (postmodern:disconnect ,g-conn)))))

(defun query (db query-string args)
  (let ((conn (postgres-conn db)))
    (cl-postgres:prepare-query conn "" query-string)
    (cl-postgres:exec-prepared conn "" args #'cl-postgres:list-row-reader)))

(defun insert-into (db table-name column-name-list values-list)
  (query db
         (with-output-to-string (s)
           (format s "INSERT INTO ~A" table-name)
           (format s " (~{~A~^,~})" column-name-list)
           (format s " VALUES ~{~A~^,~}"
            (let ((i 0)
                  (column-count (length column-name-list)))
              (loop repeat (length values-list)
                    for vals = (loop repeat column-count
                                     collect (format nil "$~A" (incf i)))
                        collect (format nil "(~{~A~^,~})" vals)))))
         (reduce #'append values-list)))


(defun parse-clause (clause)
  (let ((i 0))
    (labels ((rec (clause k)
               (if (not (keywordp (car clause)))
                   (funcall k
                    (if (null clause)
                        ""
                        (format nil "~A" clause))
                    nil)
                   (ecase (car clause)
                     (:and
                      (destructuring-bind (left right) (cdr clause)
                        (rec left
                         (lambda (l-cond l-acc-values)
                           (rec right
                            (lambda (r-cond r-acc-values)
                              (funcall k
                               (format nil "~A AND ~A" l-cond r-cond)
                               (append l-acc-values r-acc-values))))))))
                     ((:in :=)
                      (let ((op (car clause))
                            (column-name (second clause)))
                        (rec (third clause)
                         (lambda (cond acc-values)
                           (funcall k
                            (format nil "(~A ~A ~A)" column-name op cond)
                            acc-values)))))
                     (:p
                      (let ((values (alexandria:ensure-list
                                     (second clause))))
                        (funcall k
                         (format nil "(~{~A~^,~})"
                          (loop repeat (length values)
                                collect (format nil "$~A" (incf i))))
                         values)))
                     (:where
                      (rec (second clause)
                       (lambda (cond acc-values)
                         (funcall k
                          (format nil "WHERE ~A" cond)
                          acc-values))))))))
      (rec clause #'list))))

(defun delete-from (db table-name cond)
  (destructuring-bind (cond-string values) (parse-clause cond)
    (query db
           (format nil "DELETE FROM ~A ~A"
                   table-name
                   cond-string)
           values)))

(defun select-from (db column-names table-name cond &key order-by)
  (destructuring-bind (cond-string values) (parse-clause cond)
    (query db
           (with-output-to-string (s)
             (format s "SELECT ~A FROM ~A ~A"
                     column-names
                     table-name
                     cond-string)
             (when order-by
               (format s "ORDER BY ~A" order-by)))
           values)))

(defun single (row-parser select-result)
  (car (mapcar row-parser select-result)))


(defclass page (mita.page:page)
  ((id
    :initarg :id
    :type 'mita.id:id
    :reader mita.page:page-id)
   (created-on
    :initarg :created-on
    :type 'local-time:timestamp
    :reader mita.page:page-created-on)))

(defun parse-page (row)
  (make-instance 'page
   :id (mita.id:parse (first row))
   :created-on (local-time:universal-to-timestamp (second row))))


(defmethod mita.db:page-delete ((db postgres)
                                (page-id-list list))
  (when page-id-list
    (delete-from db "pages"
     `(:where (:in "page_id"
                   (:p ,(mapcar #'mita.id:to-string page-id-list)))))))

(defmethod mita.db:page-insert ((db postgres)
                                (page-id mita.id:id))
  (let ((now (local-time:now)))
    (insert-into db "pages" '("page_id" "created_on")
                 (list (list (mita.id:to-string page-id)
                             (local-time:to-rfc3339-timestring now))))))

(defmethod mita.db:page-select-by-id ((db postgres)
                                      (page-id mita.id:id))
  (single #'parse-page
          (select-from db "*" "pages"
           `(:where (:= "page_id"
                        (:p ,(mita.id:to-string page-id)))))))

(defmethod mita.db:page-select ((db postgres))
  (mapcar #'parse-page (query db "SELECT * FROM pages" nil)))


(defmethod mita.db:page-text-delete ((db postgres)
                                     (page-id-list list))
  (when page-id-list
    (delete-from db "page_text"
      `(:where (:in "page_id"
                    (:p ,(mapcar #'mita.id:to-string page-id-list)))))))

(defmethod mita.db:page-text-insert ((db postgres)
                                     (page-id mita.id:id)
                                     (text string))
  (insert-into db "page_text" '("page_id" "string")
               (list (list (mita.id:to-string page-id) text))))

(defmethod mita.db:page-text-select ((db postgres)
                                     (page-id mita.id:id))
  (single #'first
          (select-from db "string" "page_text"
           `(:where (:= "page_id" (:p ,(mita.id:to-string page-id)))))))

(defmethod mita.db:page-text-update ((db postgres)
                                     (page-id mita.id:id)
                                     (text string))
  (query db
         "UPDATE page_text set string = $1 where page_id = $2"
         (list text (mita.id:to-string page-id))))


(defun parse-image (row)
  (mita.image:make-image
   :id (mita.id:parse (first row))
   :source (alexandria:make-keyword (second row))
   :path (third row)))

(defmethod mita.db:image-select-by-ids ((db postgres)
                                        (image-id-list list))
  (mapcar #'parse-image
          (select-from db "*" "images"
           `(:where (:in "image_id"
                         (:p ,(mapcar #'mita.id:to-string
                                      image-id-list)))))))

(defmethod mita.db:image-insert ((db postgres)
                                 (images list))
  (insert-into db "images" '("image_id" "source" "path")
               (mapcar
                (lambda (image)
                  (list (mita.id:to-string (mita.image:image-id image))
                        (symbol-name (mita.image:image-source image))
                        (mita.image:image-path image)))
                images)))

(defmethod mita.db:image-delete ((db postgres)
                                 (image-id-list list))
  (when image-id-list
    (delete-from db "images"
     `(:where (:in "image_id" (:p ,(mapcar #'mita.id:to-string
                                           image-id-list)))))))

(defmethod mita.db:page-image-insert ((db postgres)
                                      (page-id mita.id:id)
                                      (images list))
  (insert-into db "page_image" '("page_id" "image_id")
               (mapcar
                (lambda (image)
                  (list (mita.id:to-string page-id)
                        (mita.id:to-string (mita.image:image-id image))))
                images)))

(defmethod mita.db:page-image-delete ((db postgres)
                                      (page-id mita.id:id))
  (delete-from db "page_image"
   `(:where (:= "page_id" (:p ,(mita.id:to-string page-id))))))

(defmethod mita.db:page-image-select ((db postgres)
                                      (page-id mita.id:id))
  (mapcar #'parse-image
          (select-from db
                       "i.image_id, i.path"
                       "images AS i
                          INNER JOIN page_image
                          ON
                            i.image_id = page_image.image_id"
           `(:where (:= "page_image.page_id"
                        (:p ,(mita.id:to-string page-id)))))))


(defmethod mita.db:album-delete ((db postgres)
                                 (album-id-list list))
  (when album-id-list
    (delete-from db "albums"
     `(:where (:in "album_id"
                   (:p ,(mapcar #'mita.id:to-string album-id-list)))))))

(defmethod mita.db:album-insert ((db postgres)
                                 (albums list))
  (insert-into db "albums" '("album_id" "name" "created_on")
               (mapcar
                (lambda (album)
                  (list (mita.id:to-string (mita.db:album-id album))
                        (mita.db:album-name album)
                        (local-time:to-rfc3339-timestring
                         (mita.db:album-created-on album))))
                albums)))

(defmethod mita.db:album-select ((db postgres)
                                 (album-id-list list))
  (when album-id-list
    (mapcar (lambda (row)
              (mita.db:make-album
               :id (mita.id:parse (first row))
               :name (second row)
               :created-on (local-time:universal-to-timestamp (third row))))
            (select-from db "*" "albums"
             `(:where (:in "album_id"
                           (:p ,(mapcar #'mita.id:to-string
                                        album-id-list))))))))

(defmethod mita.db:album-select-album-ids ((db postgres) offset limit)
  (mapcar (lambda (row)
            (mita.id:parse (car row)))
          (query db
                 "SELECT album_id FROM albums ORDER BY created_on DESC OFFSET $1 LIMIT $2"
                 (list offset limit))))


(defmethod mita.db:album-thumbnail-image-delete ((db postgres)
                                                 (album-id-list list))
  (when album-id-list
    (delete-from db "album_thumbnail_image"
                 `(:where (:in "album_id"
                   (:p ,(mapcar #'mita.id:to-string album-id-list)))))))

(defmethod mita.db:album-thumbnail-image-select ((db postgres)
                                                 (album-id-list list))
  (when album-id-list
    (mapcar (lambda (row)
              (mita.db:make-album-thumbnail-image
               :album-id (mita.id:parse (first row))
               :image-id (mita.id:parse (second row))))
            (select-from db "*" "album_thumbnail_image"
             `(:where (:in "album_id"
                           (:p ,(mapcar #'mita.id:to-string
                                        album-id-list))))))))

(defmethod mita.db:album-thumbnail-image-insert ((db postgres)
                                                 (rows list))
  (insert-into db "album_thumbnail_image" '("album_id" "image_id")
               (mapcar
                (lambda (row)
                  (list (mita.id:to-string
                         (mita.db:album-thumbnail-image-album-id row))
                        (mita.id:to-string
                         (mita.db:album-thumbnail-image-image-id row))))
                rows)))


(defmethod mita.db:album-image-insert ((db postgres)
                                       (album-id mita.id:id)
                                       (images list))
  (insert-into db "album_image" '("album_id" "image_id")
               (mapcar
                (lambda (image)
                  (list (mita.id:to-string album-id)
                        (mita.id:to-string (mita.image:image-id image))))
                images)))

(defmethod mita.db:album-image-delete ((db postgres)
                                       (album-id-list list))
  (when album-id-list
    (delete-from db "album_image"
     `(:where (:in "album_id"
                   (:p ,(mapcar #'mita.id:to-string album-id-list)))))))

(defmethod mita.db:album-image-select ((db postgres)
                                       (album-id mita.id:id))
  (mapcar #'parse-image
          (select-from db
                       "i.image_id, i.path"
                       "images AS i
                          INNER JOIN album_image
                          ON
                            i.image_id = album_image.image_id"
           `(:where (:in "album_image.album_id"
                         (:p ,(mita.id:to-string album-id)))))))


(defun parse-tag (row)
  (mita.tag:make-tag
   :id (mita.id:parse (first row))
   :name (second row)))


(defmethod mita.db:tag-delete ((db postgres)
                               (tag-id-list list))
  (when tag-id-list
    (delete-from db "tags"
     `(:where (:in "tag_id"
                   (:p ,(mapcar #'mita.id:to-string tag-id-list)))))))

(defmethod mita.db:tag-select ((db postgres))
  (mapcar #'parse-tag
          (query db "SELECT * FROM tags ORDER BY added_on" nil)))

(defmethod mita.db:tag-insert ((db postgres)
                               (tag mita.tag:tag))
  (insert-into db "tags" '("tag_id" "name" "added_on")
               (list
                (list (mita.id:to-string (mita.tag:tag-id tag))
                      (mita.tag:tag-name tag)
                      (local-time:to-rfc3339-timestring
                       (local-time:now))))))

(defmethod mita.db:tag-update ((db postgres)
                               (tag-id mita.id:id)
                               (name string))
  (query db
         "UPDATE tags SET name = $1 where tag_id = $2"
         (list name (mita.id:to-string tag-id))))

(defmethod mita.db:tag-content-delete ((db postgres)
                                       (tag-id mita.id:id))
  (delete-from db "tag_content"
   `(:where (:= "tag_id" (:p ,(mita.id:to-string tag-id))))))

(defmethod mita.db:tag-content-delete-by-content ((db postgres)
                                                  (content-id mita.id:id))
  (delete-from db "tag_content"
   `(:where (:= "content_id" (:p ,(mita.id:to-string content-id))))))

(defmethod mita.db:tag-content-select ((db postgres)
                                       (tag-id mita.id:id))
  (mapcar (lambda (row)
            (mita.db:make-content
             :id (mita.id:parse (first row))
             :type (alexandria:make-keyword (second row))))
          (select-from db "content_id, content_type" "tag_content"
           `(:where (:= "tag_id" (:p ,(mita.id:to-string tag-id))))
           :order-by "added_on")))

(defmethod mita.db:tag-content-select-tags ((db postgres)
                                            (content-id mita.id:id))
  (mapcar #'parse-tag
          (select-from db
                       "t.tag_id, t.name"
                       "tags AS t
                          INNER JOIN tag_content
                          ON
                            t.tag_id = tag_content.tag_id"
           `(:where (:= "content_id"
                        (:p ,(mita.id:to-string content-id)))))))

(defmethod mita.db:tag-content-insert ((db postgres)
                                       (tag-id mita.id:id)
                                       (contents list))
  (let ((added-on (local-time:to-rfc3339-timestring (local-time:now))))
    (insert-into db "tag_content"
                 '("tag_id" "content_type" "content_id" "added_on")
                 (mapcar
                  (lambda (content)
                    (list (mita.id:to-string tag-id)
                          (string (mita.db:content-type content))
                          (mita.id:to-string (mita.db:content-id content))
                          added-on))
                  contents))))

(defmethod mita.db:tag-content-insert-by-tags ((db postgres)
                                               (tag-id-list list)
                                               (content mita.db:content))
  (let ((added-on (local-time:to-rfc3339-timestring (local-time:now))))
    (insert-into db "tag_content"
                 '("tag_id" "content_type" "content_id" "added_on")
                 (mapcar
                  (lambda (tag-id)
                    (list (mita.id:to-string tag-id)
                          (string (mita.db:content-type content))
                          (mita.id:to-string (mita.db:content-id content))
                          added-on))
                  tag-id-list))))


;;; account

(defun parse-account (row)
  (mita.account.db:make-account
   :id (mita.id:parse (first row))
   :username (second row)
   :hashed-password (mita.account.db:make-hashed-password
                     :string (third row))))


(defmethod mita.account.db:account-insert
    ((db postgres)
     (account mita.account.db:account))
  (insert-into db "accounts" '("account_id" "username" "password_hashed")
               (list
                (list (mita.id:to-string
                       (mita.account.db:account-id account))
                      (mita.account.db:account-username
                       account)
                      (mita.account.db:hashed-password-string
                       (mita.account.db:account-hashed-password
                        account))))))

(defmethod mita.account.db:account-select ((db postgres)
                                           (username string))
  (single #'parse-account
          (select-from
           db "account_id, username, password_hashed" "accounts"
           `(:where (:= "username" (:p ,username))))))

(defmethod mita.account.db:account-select-by-id ((db postgres)
                                                 (id mita.id:id))
  (single #'parse-account
          (select-from
           db "account_id, username, password_hashed" "accounts"
           `(:where (:= "account_id" (:p ,(mita.id:to-string id)))))))
