(asdf:defsystem :mita-server-hunchentoot
  :components
  ((:file "src/server/hunchentoot"))
  :depends-on (:hunchentoot
               :jsown
               :myway
               :quri
               :mita))
