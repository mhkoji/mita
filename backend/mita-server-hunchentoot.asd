(asdf:defsystem :mita-server-hunchentoot
  :components
  ((:file "src/server/hunchentoot"))
  :depends-on (:hunchentoot
               :myway
               :quri
               :mita
               :mita-json))
