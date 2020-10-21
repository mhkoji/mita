(asdf:defsystem :mita-web
  :serial t
  :pathname "server/"

  :components
  ((:file "auth")
   (:file "auth-account")

   (:file "html")
   (:file "ningle")
   (:file "server"))

  :depends-on
  (:mita
   :mita-account

   :ningle
   :clack
   :jsown
   :cl-who
   :lack
   :lack-middleware-static
   :lack-middleware-session))
