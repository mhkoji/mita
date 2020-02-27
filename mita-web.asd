(asdf:defsystem :mita-web
  :serial t

  :pathname "mita-web/server/"
  :components
  ((:file "html")
   (:file "ningle")
   (:file "server"))

  :depends-on (:mita
               :ningle
               :clack
               :jsown
               :cl-who
               :lack
               :lack-middleware-static))
