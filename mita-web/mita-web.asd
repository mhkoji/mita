(asdf:defsystem :mita-web
  :serial t
  :pathname "server/"

  :components
  ((:file "externs")
   (:file "jsown")
   (:file "html")
   (:file "server")
   (:file "clack/mita")
   (:file "clack/log")
   (:file "clack/clack"))

  :depends-on
  (:mita
   :mita-auth

   :myway
   :clack
   :jsown
   :cl-who
   :marshal
   :lack
   :lack-middleware-static
   :lack-middleware-session))
