(asdf:defsystem :mita-auth
  :serial t
  :pathname "server-cl/"

  :components
  ((:file "auth")
   (:file "session")
   (:file "ningle")
   (:file "server"))

  :depends-on
  (:mita

   :ningle
   :cl-who
   :cl-redis
   :marshal
   :jsown
   :clack
   :lack
   :lack-middleware-session))
