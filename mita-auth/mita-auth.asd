(asdf:defsystem :mita-auth
  :serial t
  :pathname "server-cl/"

  :components
  ((:file "auth")
   (:file "ningle")
   (:file "server"))

  :depends-on
  (:mita
   :mita-account
   :ningle
   :cl-who
   :jsown
   :clack
   :lack
   :lack-middleware-session))
