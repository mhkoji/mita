(asdf:defsystem :mita-auth
  :serial t
  :pathname "server-cl/"

  :components
  ((:file "auth")
   (:file "ningle")
   (:file "server"))

  :depends-on
  (:mita

   :ningle
   :cl-who
   :jsown
   :clack
   :lack
   :lack-middleware-session
   :lack-session-store-dbi))
