(defpackage :mita.gui.ws.clack
  (:use :cl)
  (:export :start
           :init))
(in-package :mita.gui.ws.clack)

(defvar *handler* nil)

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

(defvar *db*
  (let ((locator
         (mita.db.rdb.vendor.sqlite:make-locator
          :path (system-relative-pathname "../db.sqlite"))))
    (make-instance 'mita.db.rdb.vendor.sqlite:sqlite :locator locator)))

(defvar *content-root*
  (mita.account:account-root
   (namestring (system-relative-pathname "../data/content/"))
   "7128DA4E-2B13-45CC-A0BF-78AEC1668E2C"))

(defvar *thumbnail-root*
  (mita.account:account-root
   (namestring (system-relative-pathname "../data/thumbnail/"))
   "7128DA4E-2B13-45CC-A0BF-78AEC1668E2C"))

(defun start (&key (port 16000)
                   (use-thread t)
                   (content-root *content-root*)
                   (thumbnail-root *thumbnail-root*)
                   (db *db*))
  (when *handler*
    (clack:stop *handler*))
  (setq mita.gui.ws.jsown::*account-content-root* content-root)
  (setq mita.gui.ws.jsown::*account-thumbnail-root* thumbnail-root)
  (setq *handler*
        (clack:clackup
         (lambda (env)
           (assert (string= (getf env :path-info) "/ws"))
           (let ((ws (websocket-driver:make-server env)))
             (websocket-driver:on
              :message ws (let ((gw (make-instance
                                     'mita.gui.ws.model:websocket-gateway
                                     :ws ws)))
                            (lambda (message)
                              (mita.gui.ws.model:process-message db gw message))))
             (lambda (responder)
               (declare (ignore responder))
               (websocket-driver:start-connection ws))))
         :address "0.0.0.0"
	 :debug nil
         :use-thread use-thread
         :port port)))

(defun init ()
  (let ((folders (mita.fs.dir:list-folders
                  *content-root*
                  *content-root*))
        (thumbnail-folder (mita.fs.dir:as-file
                           *thumbnail-root*
                           *thumbnail-root*)))
    (mita.db:with-connection (conn *db*)
      (mita.db:with-tx (conn)
        (mita.add-albums:run conn folders thumbnail-folder)))))
