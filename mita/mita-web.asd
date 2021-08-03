(asdf:defsystem :mita-web
  :serial t
  :pathname "src"

  :components
  ((:file "clack/util")
   (:file "web/dep")
   (:file "web/album")
   (:file "web/file")
   (:file "web/image")
   (:file "web/jsown")
   (:file "web/html")
   (:file "web/clack"))

  :depends-on
  (:mita
   :myway
   :clack
   :jsown
   :cl-who
   :marshal
   :lack
   :lack-request))
