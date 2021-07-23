(asdf:defsystem :mita-auth-backend
  :serial t

  :components
  ((:file "ningle")
   (:file "server"))

  :depends-on
  (:mita-auth

   :ningle
   :cl-who
   :jsown
   :clack
   :lack
   :lack-middleware-session))
