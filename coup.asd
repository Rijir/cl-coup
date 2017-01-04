(defsystem coup
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
  :in-order-to ((test-op (test-op coup/test))))

(defsystem coup/manual-player
  :description "functions to make a player which uses standard input to make
                decisions"
  :depends-on (:coup)
  :components ((:file "manual-player/manual-player")))

(defsystem coup/test
  :depends-on (:coup
               :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:file "test/package"))
  :in-order-to ((test-op (test-op coup/test/card
                                  coup/test/action))))

(defsystem coup/test/card
  :depends-on (:coup/test)
  :defsystem-depends-on (:prove-asdf)
  :components ((:test-file "test/card-tests"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))

(defsystem coup/test/action
  :depends-on (:coup/test)
  :defsystem-depends-on (:prove-asdf)
  :components ((:test-file "test/action-tests"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
