(asdf:defsystem :mita-web
  :serial t
  :pathname "server/"

  :components
  ((:file "externs")
   (:file "auth")
   (:file "jsown")
   (:file "html")
   (:file "ningle")
   (:file "server"))

  :depends-on
  (:mita
   :mita-auth

   :ningle
   :clack
   :jsown
   :cl-who
   :marshal
   :lack
   :lack-middleware-static
   :lack-middleware-session))
