(asdf:defsystem :mita-server-ningle
  :components
  ((:file "src/server/ningle"))
  :depends-on (:clack
               :ningle
               :mita))
