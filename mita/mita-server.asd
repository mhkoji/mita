(asdf:defsystem :mita-server
  :serial t
  :pathname "server/server/"

  :components
  ((:file "externs")
   (:file "jsown")
   (:file "html")
   (:file "app")
   (:file "clack/mita")
   (:file "clack/clack"))

  :depends-on
  (:mita-util-auth
   :mita-util-clack
   :mita-auth-server
   :mita

   :myway
   :clack
   :jsown
   :cl-who
   :marshal
   :lack
   :lack-middleware-static
   :lack-middleware-session))
