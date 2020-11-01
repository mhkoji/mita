(asdf:defsystem :mita-web
  :serial t
  :pathname "server/"

  :components
  ((:file "externs")
   (:file "auth")
   (:file "html")
   (:file "ningle")
   (:file "server"))

  :depends-on
  (:mita
   :mita-account
   :mita-auth

   :ningle
   :clack
   :jsown
   :cl-who
   :lack
   :lack-middleware-static
   :lack-middleware-session))
