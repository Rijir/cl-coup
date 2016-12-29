(defsystem "coup"
  :description "coup: an engine for the card game Coup, along with hopefully a great AI"
  :version "0.0.1"
  :author "Timothy Trindle <ttrindle@uw.edu>"
  :licence "TBD"
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "cards" :depends-on ("package"))
               (:file "player" :depends-on ("package"))
               (:file "game" :depends-on ("player"))
               (:file "actions" :depends-on ("cards" "game"))
               (:file "coup" :depends-on ("actions")))
  :in-order-to ((test-op (test-op "coup/test"))))

(defsystem "coup/test"
  :depends-on ("coup" :sb-rt)
  :components ((:file "test/coup-tests"))
  :perform (test-op (o s)
                    (uiop:symbol-call :sb-rt '#:do-tests)))
