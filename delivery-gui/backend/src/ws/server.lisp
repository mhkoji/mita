(defpackage :mita.gui.ws.server
  (:use :cl)
  (:export :*db-path*
           :*content-root*
           :*thumbnail-root*
           :start
           :init))
(in-package :mita.gui.ws.server)

(defvar *handler* nil)

(defvar *db-path*
  "db.sqlite")

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

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
             (websocket-driver:on
              :message ws (let ((db (make-instance
                                     'mita.db.vendor.sqlite:sqlite
                                     :locator (make-locator)))
                                (gw (make-instance
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
  (let ((locator (make-locator)))
    (mita.db.vendor.sqlite:drop-database
     locator)
    (mita.db.vendor.sqlite:create-database
     (system-relative-pathname "../sqlite/") locator)
    (let ((folders (mita.fs.dir:list-folders *content-root*
                                             *content-root*))
          (thumbnail-folder (mita.fs.dir:as-file *thumbnail-root*
                                               *thumbnail-root*)))
      (mita.db:with-connection (conn (make-instance
                                      'mita.db.vendor.sqlite:sqlite
                                      :locator locator))
        (mita.db:with-tx (conn)
          (mita.add-albums:run conn folders thumbnail-folder))))))
