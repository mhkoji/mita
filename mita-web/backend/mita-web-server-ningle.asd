(asdf:defsystem :mita-web-server-ningle
  :components
  ((:file "src/server/ningle"))
  :depends-on (:clack
               :jsown
               :ningle
               :mita-web))
