(asdf:defsystem :mita-web-backend
  :serial t

  :components
  ((:file "externs")
   (:file "jsown")
   (:file "html")
   (:file "app")
   (:file "clack/util")
   (:file "clack/mita")
   (:file "clack/clack"))

  :depends-on
  (:mita-util-auth
   :mita-util-clack
   :mita-auth-backend
   :mita

   :hunchentoot
   :myway
   :clack
   :jsown
   :cl-who
   :marshal
   :lack
   :lack-middleware-static
   :lack-middleware-session))
