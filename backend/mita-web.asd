(asdf:defsystem :mita-web
  :serial t
  :pathname "src/web"
  :components
  ((:file "html")
   (:file "ningle"))
  :depends-on (:cl-who
               :clack
               :jsown
               :ningle
               :mita))