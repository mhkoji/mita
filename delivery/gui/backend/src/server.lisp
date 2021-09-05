(defpackage :mita.delivery.gui.server
  (:use :cl)
  (:export :*db-path*
           :*content-root*
           :*thumbnail-root*
           :start
           :init))
(in-package :mita.delivery.gui.server)

(defvar *handler* nil)

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

(defvar *db-path*
  (namestring
   (system-relative-pathname "../db.sqlite3")))

(defvar *content-root*
  (namestring
   (system-relative-pathname
    "../data/content/account_7128da4e_2b13_45cc_a0bf_78aec1668e2c")))

(defvar *thumbnail-root*
  (namestring
   (system-relative-pathname
    "../data/thumbnail/account_7128da4e_2b13_45cc_a0bf_78aec1668e2c")))

(defun make-locator ()
  (mita.db.vendor.sqlite:make-locator :path *db-path*))

(defvar *gws* nil)

(defun start (&key (port 16000)
                   (use-thread t))
  (when *handler*
    (clack:stop *handler*))
  (setq mita.gui.ws.jsown::*content-root* *content-root*)
  (setq mita.gui.ws.jsown::*thumbnail-root* *thumbnail-root*)
  (setq *handler*
        (clack:clackup
         (lambda (env)
           (assert (string= (getf env :path-info) "/ws"))
           (let ((ws (websocket-driver:make-server env)))
             (let ((db (make-instance
                        'mita.db.vendor.sqlite:sqlite
                        :locator (make-locator)))
                   (gw (make-instance
                        'mita.gui.ws.model:websocket-gateway
                        :ws ws)))
               (websocket-driver:on
                :message ws (mita.gui.ws.model:create-listener db gw))
               (push gw *gws*))
             (lambda (responder)
               (declare (ignore responder))
               (websocket-driver:start-connection ws))))
         :address "0.0.0.0"
	 :debug nil
         :use-thread use-thread
         :port port)))

(defun init ()
  (let ((locator (make-locator)))
    (mita.db.vendor.sqlite:drop-database
     locator)
    (mita.db.vendor.sqlite:create-database
     (system-relative-pathname "../sqlite/") locator)
    (let ((content-repos (make-instance 'mita.file.fs:repository
                                        :root *content-root*))
          (thumbnail-repos (make-instance 'mita.file.fs:repository
                                          :root *thumbnail-root*)))
      (let ((folders (mita.file.fs:list-folders content-repos "/")))
        (when folders
          (mita.db:with-connection (conn (make-instance
                                          'mita.db.vendor.sqlite:sqlite
                                          :locator locator))
            (mita.db:with-tx (conn)
              (mita.album.add:run
               conn thumbnail-repos content-repos folders))))))))
