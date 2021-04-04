(defpackage :mita.timezone
  (:use :cl))
(in-package :mita.timezone)

(setq local-time:*default-timezone* local-time:+utc-zone+)
