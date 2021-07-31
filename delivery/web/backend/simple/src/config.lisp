(defpackage :mita.delivery.web.simple.config
  (:use :cl))
(in-package :mita.delivery.web.simple.config)

  ;; In oreder not to install ssl on Windows.
(push :hunchentoot-no-ssl *features*)

;; (setq mita.delivery.web.simple::*db-path*)

;; (setq mita.delivery.web.simple::*static-root*)

;; (setq mita.delivery.web.simple::*content-root*)

;; (setq mita.delivery.web.simple::*thumbnail-root*)
