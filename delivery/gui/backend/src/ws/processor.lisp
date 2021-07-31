(defpackage :mita.gui.ws.model
  (:use :cl)
  (:export :websocket-gateway
           :process-message))
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

(defmethod mita.gui.album-list:update-state ((gw websocket-gateway) state)
  (update-state gw :album-list state))

(defmethod mita.gui.album:update-state ((gw websocket-gateway) state)
  (update-state gw :album state))

(defmethod mita.gui.tag-edit:update-state ((gw websocket-gateway) state)
  (update-state gw :tag-edit state))

(defun process-message (db gw message)
  (let* ((msg (jsown:parse message))
         (op (jsown:val msg "op"))
         (states (slot-value gw 'states)))
    (cond ((string= op "album-list:list-albums")
           (let ((limit (jsown:val msg "limit")))
             (mita.gui.album-list:list-albums 0 limit db gw)))

          ((string= op "album-list:next")
           (let ((state (gethash :album-list states)))
             (assert (typep state 'mita.gui.album-list:listed))
             (mita.gui.album-list:next-albums state db gw)))

          ((string= op "album-list:prev")
           (let ((state (gethash :album-list states)))
             (assert (typep state 'mita.gui.album-list:listed))
             (mita.gui.album-list:prev-albums state db gw)))


          ((string= op "album:list-images")
           (let ((album-id (mita.id:parse-short (jsown:val msg "album-id"))))
             (mita.gui.album:list-images album-id db gw)))

          ((string= op "album:view")
           (let ((state (gethash :album states)))
             (assert (typep state 'mita.gui.album:album))
             (let ((image-id (alexandria:when-let
                                 ((str (jsown:val-safe msg "image-id")))
                               (mita.id:parse-short str))))
               (mita.gui.album:view state image-id gw))))

          ((string= op "album:view-set-index")
           (let ((state (gethash :album states)))
             (assert (typep state 'mita.gui.album:viewing))
             (let ((image-id (mita.id:parse-short (jsown:val msg "image-id"))))
               (mita.gui.album:set-index state image-id gw))))

          ((string= op "album:view-diff")
           (let ((state (gethash :album states)))
             (assert (typep state 'mita.gui.album:viewing))
             (let ((diff (jsown:val msg "diff")))
               (mita.gui.album:increment-index state diff gw))))

          
          ((string= op "tag-edit:start")
           (let ((album-id (mita.id:parse-short (jsown:val msg "album-id"))))
             (labels ((load-content (conn)
                        (mita.album:load-album-by-id conn album-id)))
               (mita.gui.tag-edit:start #'load-content db gw))))

          ((string= op "tag-edit:add-tag")
           (let ((state (gethash :tag-edit states)))
             (assert (typep state 'mita.gui.tag-edit:editing))
             (let ((name (jsown:val msg "name")))
             (mita.gui.tag-edit:add-tag state name db gw))))

          ((string= op "tag-edit:delete-tag")
           (let ((state (gethash :tag-edit states)))
             (assert (typep state 'mita.gui.tag-edit:editing))
             (let ((tag-id (mita.id:parse-short (jsown:val msg "tag-id"))))
               (mita.gui.tag-edit:delete-tag state tag-id db gw))))

          ((string= op "tag-edit:attach-tag")
           (let ((state (gethash :tag-edit states)))
             (assert (typep state 'mita.gui.tag-edit:editing))
             (let ((tag-id (mita.id:parse-short (jsown:val msg "tag-id"))))
               (mita.gui.tag-edit:attach-tag state tag-id gw))))

          ((string= op "tag-edit:detach-tag")
           (let ((state (gethash :tag-edit states)))
             (assert (typep state 'mita.gui.tag-edit:editing))
             (let ((tag-id (mita.id:parse-short (jsown:val msg "tag-id"))))
               (mita.gui.tag-edit:detach-tag state tag-id gw))))

          ((string= op "tag-edit:save-content-tags")
           (let ((state (gethash :tag-edit states)))
             (assert (typep state 'mita.gui.tag-edit:editing))
             (mita.gui.tag-edit:save state db gw)))

          ((string= op "tag-edit:stop")
           (mita.gui.tag-edit:stop gw))

          (t
           (print msg *standard-output*)))))
