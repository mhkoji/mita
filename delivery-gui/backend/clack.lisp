(defpackage :mita.gui.clack
  (:use :cl)
  (:export :start))
(in-package :mita.gui.clack)

(defvar *locator* (mita.db.impl:make-locator))

(defvar *handler* nil)

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))


(defun html ()
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href "/static/gen/index.bundle.css"))
    (:body
     (:div :id "app")
     (:div :id "app-modal")
     (:script
      :type "text/javascript"
      :src "/static/gen/index.bundle.js"))))

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

(defmethod mita.web.app:request-account-id ((req req))
  "7128DA4E-2B13-45CC-A0BF-78AEC1668E2C")

(defun start (&key (port 6000)
                   (use-thread t)
                   (static-root
                    (system-relative-pathname "../delivery-web/mita/static/"))
                   (content-base
                    (cl-fad:directory-exists-p
                     (system-relative-pathname "../data/content/")))
                   (thumbnail-base
                    (cl-fad:directory-exists-p
                     (system-relative-pathname "../data/thumbnail/")))
                   (locator *locator*))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler*
        (clack:clackup
         (lack:builder
          (:static :path "/static/" :root static-root)
          (lambda (env)
            (let ((req (make-req))
                  (path-info (getf env :path-info)))
              (cond ((string= path-info "/")
                     `(200 (:content-type "text/html") (,(html))))
                    ((cl-ppcre:scan "^/images/" path-info)
                     (mita.web.app:image-serve
                      (make-instance 'mita.web.app:spec
                       :locator locator
                       :content-base (namestring content-base)
                       :thumbnail-base (namestring thumbnail-base))
                      req
                      ;; (length "/images/")
                      (subseq path-info 8)
                      :on-found (lambda (path) `(200 () ,path))
                      :on-not-found (lambda () nil)))
                    ((string= path-info "/ws")
                     (let ((ws (websocket-driver:make-server env))
                           (db (mita.web.app:make-db req locator)))
                       (websocket-driver:on
                        :message ws (lambda (message)
                                      (process-message db ws message)))
                       (lambda (responder)
                         (declare (ignore responder))
                         (websocket-driver:start-connection ws))))
                    (t
                     '(302 (:location "/") nil))))))
         :address "0.0.0.0"
	 :debug nil
         :use-thread use-thread
         :port port)))
