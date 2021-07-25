(defpackage :mita.gui.clack
  (:use :cl)
  (:export :start))
(in-package :mita.gui.clack)

(defvar *locator* (mita.db.impl:make-locator))

(defvar *handler* nil)

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

(defclass websocket-gateway ()
  ((ws :initarg :ws)))

(defvar *state* nil)

(defun process-message (db ws message)
  (let ((msg (jsown:parse message)))
    (let ((op (jsown:val msg "op")))
      (cond ((string= op "view-albums")
             (mita.gui:view-albums
              db
              (jsown:val msg "offset")
              (jsown:val msg "limit")
              (make-instance 'websocket-gateway :ws ws)))))))

(defmethod mita.gui:update-state ((gw websocket-gateway) state)
  (setq *state* state)
  (let ((resp (jsown:to-json state)))
    (print resp)
    (websocket-driver:send (slot-value gw 'ws) resp)))


(defstruct req)

(defvar *account-id* "7128DA4E-2B13-45CC-A0BF-78AEC1668E2C")

(defmethod mita.web.app:request-account-id ((req req))
  *account-id*)

(defun start (&key (port 6000)
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
                :message ws (lambda (message)
                              (process-message db ws message)))
               (lambda (responder)
                 (declare (ignore responder))
                 (websocket-driver:start-connection ws)))))
         :address "0.0.0.0"
	 :debug nil
         :use-thread use-thread
         :port port)))
