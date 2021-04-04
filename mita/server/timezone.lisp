(defpackage :mita.timezone
  (:use :cl))
(in-package :mita.timezone)

(local-time:reread-timezone-repository)
(setq local-time:*default-timezone*
      (local-time:find-timezone-by-location-name "Asia/Tokyo"))
