(asdf:defsystem :mita-util-auth
  :serial t
  :pathname "auth/"

  :components
  ((:file "auth")
   (:file "lack")
   (:file "session"))

  :depends-on
  (:cl-base64
   :cl-bcrypt
   :cl-redis
   :marshal
   :lack
   :lack-middleware-session
   :trivial-utf-8))
