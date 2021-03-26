(asdf:defsystem :mita-util-auth
  :serial t
  :pathname "auth/"

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
