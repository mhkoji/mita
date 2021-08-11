(defpackage :mita.gui.ws.model
  (:use :cl)
  (:export :websocket-gateway
           :create-listener))
(in-package :mita.gui.ws.model)

(defclass websocket-gateway ()
  ((ws
    :initarg :ws)
   (states
    :initform (make-hash-table))))

(defun update-state (gw state-category state)
  (if (null state)
      (remhash state-category (slot-value gw 'states))
      (setf (gethash state-category (slot-value gw 'states)) state))
  (let ((view (mita.gui.view:to-json state-category state)))
    (websocket-driver:send (slot-value gw 'ws) view)))

(defun get-state (gw state-category)
  (gethash state-category (slot-value gw 'states)))

(defmethod mita.gui.album-list:update-state ((gw websocket-gateway) state)
  (update-state gw :album-list state))

(defmethod mita.gui.album:update-state ((gw websocket-gateway) state)
  (update-state gw :album state))

(defmethod mita.gui.tag-edit:update-state ((gw websocket-gateway) state)
  (update-state gw :tag-edit state))

(defun album-list (db gw)
  (list
   (list
    "album-list:list-albums"
    (lambda (args)
      (let ((limit (jsown:val args "limit")))
        (mita.gui.album-list:list-albums 0 limit db gw))))
   (list
    "album-list:next"
    (lambda (args)
      (declare (ignore args))
      (let ((state (get-state gw :album-list)))
        (assert (typep state 'mita.gui.album-list:listed))
        (mita.gui.album-list:next-albums state db gw))))
   (list
    "album-list:prev"
    (lambda (args)
      (declare (ignore args))
      (let ((state (get-state gw :album-list)))
        (assert (typep state 'mita.gui.album-list:listed))
        (mita.gui.album-list:prev-albums state db gw))))))

(defun album (db gw)
  (list
   (list
    "album:list-images"
    (lambda (args)
      (let ((album-id (mita.id:parse-short (jsown:val args "album-id"))))
        (mita.gui.album:list-images album-id db gw))))
   (list
    "album:view"
    (lambda (args)
      (let ((state (get-state gw :album)))
        (assert (typep state 'mita.gui.album:album))
        (let ((image-id (alexandria:when-let
                            ((str (jsown:val-safe args "image-id")))
                          (mita.id:parse-short str))))
          (mita.gui.album:view state image-id gw)))))
   (list
    "album:view-set-index"
    (lambda (args)
      (let ((state (get-state gw :album)))
        (assert (typep state 'mita.gui.album:viewing))
        (let ((image-id (mita.id:parse-short (jsown:val args "image-id"))))
          (mita.gui.album:set-index state image-id gw)))))
   (list
    "album:view-diff"
    (lambda (args)
      (let ((state (get-state gw :album)))
        (assert (typep state 'mita.gui.album:viewing))
        (let ((diff (jsown:val args "diff")))
          (mita.gui.album:increment-index state diff gw)))))))

(defun tag-edit (db gw)
  (list
   (list
    "tag-edit:start"
    (lambda (args)
      (let ((album-id (mita.id:parse-short (jsown:val args "album-id"))))
        (labels ((load-content (conn)
                   (mita.album:load-album-by-id conn album-id)))
          (mita.gui.tag-edit:start #'load-content db gw)))))
   (list
    "tag-edit:add-tag"
    (lambda (args)
      (let ((state (get-state gw :tag-edit)))
        (assert (typep state 'mita.gui.tag-edit:editing))
        (let ((name (jsown:val args "name")))
          (mita.gui.tag-edit:add-tag state name db gw)))))
   (list
    "tag-edit:delete-tag"
    (lambda (args)
      (let ((state (get-state gw :tag-edit)))
        (assert (typep state 'mita.gui.tag-edit:editing))
        (let ((tag-id (mita.id:parse-short (jsown:val args "tag-id"))))
          (mita.gui.tag-edit:delete-tag state tag-id db gw)))))
   (list
    "tag-edit:attach-tag"
    (lambda (args)
      (let ((state (get-state gw :tag-edit)))
        (assert (typep state 'mita.gui.tag-edit:editing))
        (let ((tag-id (mita.id:parse-short (jsown:val args "tag-id"))))
          (mita.gui.tag-edit:attach-tag state tag-id gw)))))
   (list 
    "tag-edit:detach-tag"
    (lambda (args)
      (let ((state (get-state gw :tag-edit)))
        (assert (typep state 'mita.gui.tag-edit:editing))
        (let ((tag-id (mita.id:parse-short (jsown:val args "tag-id"))))
          (mita.gui.tag-edit:detach-tag state tag-id gw)))))
   (list 
    "tag-edit:save-content-tags"
    (lambda (args)
      (declare (ignore args))
      (let ((state (get-state gw :tag-edit)))
        (assert (typep state 'mita.gui.tag-edit:editing))
        (mita.gui.tag-edit:save state db gw))))
   (list 
    "tag-edit:stop"
    (lambda (args)
      (declare (ignore args))
      (mita.gui.tag-edit:stop gw)))))

(defun create-listener (db gw)
  (let ((op-callback-hash (make-hash-table :test #'equal)))
    (loop for (op callback) in (nconc (album-list db gw)
                                      (album db gw)
                                      (tag-edit db gw))
          do (setf (gethash op op-callback-hash) callback))

    (lambda (message)
      (let ((msg (jsown:parse message)))
        (let ((callback (gethash (jsown:val msg "op")
                                 op-callback-hash)))
          (if callback
              (funcall callback msg)
              (print msg *standard-output*)))))))
