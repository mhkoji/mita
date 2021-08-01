(defpackage :mita.gui.tag-edit
  (:use :cl)
  (:export :update-state
           :loading
           :editing
           :editing-is-tag-added
           :editing-tags
           :editing-content-tags
           :saving
           :saved
           :start
           :stop
           :add-tag
           :delete-tag
           :attach-tag
           :detach-tag
           :save))
(in-package :mita.gui.tag-edit)

(defgeneric update-state (gateway state))

(defstruct (loading (:include mita.gui.state:loading)))

(defstruct editing
  content
  tags
  content-tags
  is-tag-added) ;; notify to frontend that the input box should be cleaned up.

(defstruct saving)

(defstruct saved)

(defun start (load-content-fn db gw)
  (update-state gw (make-loading))

  (mita.db:with-connection (conn db)
    (let ((tags (mita.tag:load-tags conn)))
      (let ((content (funcall load-content-fn conn)))
        (let ((content-tags (mita.tag:content-tags conn content)))
          (update-state gw (make-editing
                            :content content
                            :tags tags
                            :content-tags content-tags
                            :is-tag-added nil)))))))

(defun stop (gw)
  (update-state gw nil))

(defun add-tag (editing name db gw)
  (mita.db:with-connection (conn db)
    (mita.tag:do-create-tag (push-tag conn)
      (push-tag name))
    (setf (editing-tags editing) (mita.tag:load-tags conn)))
  (setf (editing-is-tag-added editing) t)
  (update-state gw editing)

  (setf (editing-is-tag-added editing) nil)
  (update-state gw editing))

(defun delete-tag (editing tag-id db gw)
  (mita.db:with-connection (conn db)
    (mita.tag:delete-tag conn tag-id)
    (setf (editing-tags editing) (mita.tag:load-tags conn)))
  (update-state gw editing))

(defun attach-tag (editing tag-id gw)
  (let ((tag (find tag-id (editing-tags editing)
                   :key #'mita.tag:tag-id
                   :test #'mita.id:id=)))
    (when tag
      (push tag (editing-content-tags editing))
      (update-state gw editing))))

(defun detach-tag (editing tag-id gw)
  (alexandria:deletef (editing-content-tags editing) tag-id
                      :key #'mita.tag:tag-id
                      :test #'mita.id:id=)
  (update-state gw editing))

(defun save (editing db gw)
  (update-state gw (make-saving))
  (mita.db:with-connection (conn db)
    (let ((content (editing-content editing))
          (tag-ids (mapcar #'mita.tag:tag-id
                           (editing-content-tags editing))))
      (mita.tag:update-content-tags conn content tag-ids)
      (update-state gw (make-saved))
      (sleep 1)
      (stop gw))))
