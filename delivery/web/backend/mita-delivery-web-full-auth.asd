(asdf:defsystem :mita-delivery-web-full-auth
  :serial t
  :pathname "full/auth"

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
