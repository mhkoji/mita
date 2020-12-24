(defpackage :mita.timezone
  (:use :cl))
(in-package :mita.timezone)

(local-time:reread-timezone-repository)
(local-time:find-timezone-by-location-name "Asia/Tokyo")
