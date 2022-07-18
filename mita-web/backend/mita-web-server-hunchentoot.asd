(asdf:defsystem :mita-web-server-hunchentoot
  :components
  ((:file "src/server/hunchentoot"))
  :depends-on (:hunchentoot
               :myway
               :quri
               :mita-web))
