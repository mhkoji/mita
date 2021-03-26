(asdf:defsystem :mita-server
  :serial t
  :pathname "server/server/"

  :components
  ((:file "externs")
   (:file "jsown")
   (:file "html")
   (:file "app")
   (:file "clack/mita")
   (:file "clack/log")
   (:file "clack/clack"))

  :depends-on
  (:mita
   :mita-util-auth
   :mita-auth-server

   :myway
   :clack
   :jsown
   :cl-who
   :marshal
   :lack
   :lack-middleware-static
   :lack-middleware-session))
