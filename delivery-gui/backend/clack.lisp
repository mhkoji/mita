(defpackage :mita.gui.clack
  (:use :cl)
  (:export :start))
(in-package :mita.gui.clack)

(defvar *locator* (mita.db.impl:make-locator))

(defvar *handler* nil)

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

(defclass state-holder ()
  ((state
    :initform nil
    :accessor state-holder-state)))

(defmethod mita.gui:update-state ((gw state-holder) state)
  (setf (state-holder-state gw) state))

(defclass websocket-gateway (state-holder)
  ((ws
    :initarg :ws)))

(defmethod mita.gui:update-view ((gw websocket-gateway) (view mita.gui.view:view))
  (websocket-driver:send (slot-value gw 'ws)
                         (let ((state (state-holder-state gw)))
                           (let ((state-name
                                  (string-downcase (symbol-name (type-of state)))))
                             (mita.gui.view:view-json state-name view)))))

(defun process-message (db gw message)
  (let* ((msg (jsown:parse message))
         (op (jsown:val msg "op"))
         (state (state-holder-state gw)))
    (cond ((string= op "list-albums")
           (let ((limit (jsown:val msg "limit")))
             (mita.gui:list-albums db 0 limit gw)))
          ((string= op "next-albums")
           (assert (typep state 'mita.gui.state:album-list))
           (let ((offset (mita.gui.state:album-list-next-offset state))
                 (limit (mita.gui.state:album-list-limit state)))
             (mita.gui:list-albums db offset limit gw)))
          ((string= op "prev-albums")
           (assert (typep state 'mita.gui.state:album-list))
           (let ((offset (mita.gui.state:album-list-prev-offset state))
                 (limit (mita.gui.state:album-list-limit state)))
             (mita.gui:list-albums db offset limit gw)))
          ((string= op "select-album")
           (let ((album-id (mita.id:parse-short (jsown:val msg "album-id"))))
             (mita.gui:select-album db album-id gw))))))


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
  (setq mita.gui.jsown::*account-content-root*
        (mita.account:account-root (namestring content-base) *account-id*))
  (setq mita.gui.jsown::*account-thumbnail-root*
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
