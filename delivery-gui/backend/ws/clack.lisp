(defpackage :mita.gui.ws.clack
  (:use :cl)
  (:export :start))
(in-package :mita.gui.ws.clack)

(defvar *locator* (mita.db.impl:make-locator))

(defvar *handler* nil)

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

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


(defstruct req)

(defvar *account-id* "7128DA4E-2B13-45CC-A0BF-78AEC1668E2C")

(defmethod mita.web.app:request-account-id ((req req))
  *account-id*)

(defun start (&key (port 16000)
                   (use-thread t)
                   (content-base
                    (cl-fad:directory-exists-p
                     (system-relative-pathname "../data/content/")))
                   (thumbnail-base
                    (cl-fad:directory-exists-p
                     (system-relative-pathname "../data/thumbnail/")))
                   (locator *locator*))
  (when *handler*
    (clack:stop *handler*))
  (setq mita.gui.ws.jsown::*account-content-root*
        (mita.account:account-root (namestring content-base) *account-id*))
  (setq mita.gui.ws.jsown::*account-thumbnail-root*
        (mita.account:account-root (namestring thumbnail-base) *account-id*))
  (setq *handler*
        (clack:clackup
         (lambda (env)
           (assert (string= (getf env :path-info) "/ws"))
           (let ((req (make-req)))
             (let ((ws (websocket-driver:make-server env))
                   (db (mita.web.app:make-db req locator)))
               (websocket-driver:on
                :message ws (let ((gw (make-instance 'websocket-gateway :ws ws)))
                              (lambda (message)
                                (process-message db gw message))))
               (lambda (responder)
                 (declare (ignore responder))
                 (websocket-driver:start-connection ws)))))
         :address "0.0.0.0"
	 :debug nil
         :use-thread use-thread
         :port port)))
