(asdf:defsystem :mita-web
  :serial t
  :pathname "src"

  :components
  ((:file "clack/util")
   (:file "web/app")
   (:file "web/jsown")
   (:file "web/html")
   (:file "web/clack"))

  :depends-on
  (:mita
   :hunchentoot
   :myway
   :clack
   :jsown
   :cl-who
   :marshal
   :lack
   :lack-request))
