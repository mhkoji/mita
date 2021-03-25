(asdf:defsystem :mita-auth
  :serial t
  :pathname "server-cl/auth/"

  :components
  ((:file "auth")
   (:file "lack")
   (:file "session"))

  :depends-on
  (:mita-admin

   :marshal
   :cl-redis
   :lack
   :lack-middleware-session))
