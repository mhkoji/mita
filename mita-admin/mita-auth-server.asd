(asdf:defsystem :mita-auth-server
  :serial t
  :pathname "server-cl/server/"

  :components
  ((:file "ningle")
   (:file "server"))

  :depends-on
  (:mita-admin
   :mita-util-auth

   :ningle
   :cl-who
   :jsown
   :clack
   :lack
   :lack-middleware-session))
