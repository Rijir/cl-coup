(in-package :coup-tests)

(plan 4)

(let* ((p1 (make-instance 'player
                          :id 'foo
                          :cards (list *assassin* *contessa*)
                          :money 3))
       (p1-original (copy-player p1))
       (p2 (make-instance 'player
                          :id 'bar
                          :cards (list *duke* *ambassador*)
                          :money 7))
       (p2-original (copy-player p2))
       (p3 (make-instance 'player
                          :id 'baz
                          :cards (list *sargent* *contessa*)
                          :money 1))
       (p3-original (copy-player p3))
       (g (make-game :players (list p1 p2 p3) :deck (make-deck))))
  (subtest "Testing action-is-valid"
    (plan 6)
    ;; these are always valid
    (subtest "Always valid actions"
      (plan 3)
      (ok (action-is-valid (make-instance 'income :by p1))
          "income action is valid")
      (ok (action-is-valid (make-instance 'foreign-aid :by p1))
          "foreign aid action is valid")
      (ok (action-is-valid (make-instance 'coup :by p1 :target p2))
          "coup action is valid")
      (finalize))

    ;; Tax action validity
    ;; valid if player has duke
    (subtest "Tax"
      (plan 3)
      (ok (not (action-is-valid (make-instance 'tax :by p1)))
          "tax by p1 is not valid")
      (ok (action-is-valid (make-instance 'tax :by p2))
          "tax by p2 is valid")
      (ok (not (action-is-valid (make-instance 'tax :by p3)))
          "tax by p3 is not valid")
      (finalize))

    ;; Exchange action validity
    ;; valid if player has ambassador
    (subtest "Exchange"
      (plan 3)
      (ok (not (action-is-valid (make-instance 'exchange :by p1)))
          "exchange by p1 is not valid")
      (ok (action-is-valid (make-instance 'exchange :by p2))
          "exchange by p2 is valid")
      (ok (not (action-is-valid (make-instance 'exchange :by p3)))
          "exchange by p3 is not valid")
      (finalize))

    ;; Assassinate action validity
    ;; valid if player has assassin
    (subtest "Assassinate"
      (plan 3)
      (ok (action-is-valid (make-instance 'assassinate :by p1 :target p2))
          "assassinate by p1 is valid")
      (ok (not (action-is-valid (make-instance 'assassinate :by p2 :target p1)))
          "assassinate by p2 is not valid")
      (ok (not (action-is-valid (make-instance 'assassinate :by p3 :target p1)))
          "assassinate by p3 is not valid")
      (finalize))

    ;; Steal action validity
    ;; valid if player has sargent
    (subtest "Steal"
      (plan 3)
      (ok (not (action-is-valid (make-instance 'steal :by p1 :target p2)))
          "steal by p1 is not valid")
      (ok (not (action-is-valid (make-instance 'steal :by p2 :target p3)))
          "steal by p2 is not valid")
      (ok (action-is-valid (make-instance 'steal :by p3 :target p2))
          "steal by p3 is valid")
      (finalize))

    (subtest "Block"
      (plan 3)
      ;; Block assassinate validity
      ;; valid if blocking player has contessa
      ;; assassinate does not have to be valid for block to be valid
      (subtest "Assassinate"
        (plan 3)
        (ok (action-is-valid (make-instance 'block-action
                                            :by p1
                                            :target (make-instance 'assassinate
                                                                   :by p3
                                                                   :target p1)))
            "p1's block of assassinate is valid")
        (ok (not (action-is-valid (make-instance 'block-action
                                                 :by p2
                                                 :target (make-instance 'assassinate
                                                                        :by p1
                                                                        :target p2))))
            "p2's block of assassinate is not valid")
        (ok (action-is-valid (make-instance 'block-action
                                            :by p3
                                            :target (make-instance 'assassinate
                                                                   :by p1
                                                                   :target p3)))
            "p3's block of assassinate is valid")
        (finalize))

      ;; Block foreign aid validity
      ;; valid if blocking player has duke
      (subtest "Foreign Aid"
        (plan 3)
        (ok (not (action-is-valid (make-instance 'block-action
                                                 :by p1
                                                 :target (make-instance 'foreign-aid
                                                                        :by p2))))
            "p1's block of foreign aid is not valid")
        (ok (action-is-valid (make-instance 'block-action
                                            :by p2
                                            :target (make-instance 'foreign-aid
                                                                   :by p1)))
            "p2's block of foreign aid is valid")
        (ok (not (action-is-valid (make-instance 'block-action
                                                 :by p3
                                                 :target (make-instance 'foreign-aid
                                                                        :by p1))))
            "p3's block of foreign aid is not valid")
        (finalize))

      ;; Block steal validity
      ;; valid if blocking player has sargent or ambassador
      (subtest "Steal"
        (plan 3)
        (ok (not (action-is-valid (make-instance 'block-action
                                                 :by p1
                                                 :target (make-instance 'steal
                                                                        :by p2
                                                                        :target p1))))
            "p1's block of steal is not valid")
        (ok (action-is-valid (make-instance 'block-action
                                            :by p2
                                            :target (make-instance 'steal
                                                                   :by p3
                                                                   :target p2)))
            "p2's block of steal is valid (ambassador)")
        (ok (action-is-valid (make-instance 'block-action
                                            :by p3
                                            :target (make-instance 'steal
                                                                   :by p1
                                                                   :target p3)))
            "p3's block of steal is valid (sargent)")
        (finalize))
      (finalize)))

  (defmethod player-block? (p a g) nil)
  (defmethod player-contest? (p a g) nil)
  (defmethod player-exchange ((p (eql p1)) cs g)
    (subseq cs 1 3))
  (defmethod player-lose-card ((p (eql p1)) g)
    (first (player-cards p)))
  (defmethod player-lose-card ((p (eql p2)) g)
    (second (player-cards p)))
  (subtest "Apply action"
    (plan 8)
    (let ((prove:*default-test-function* #'players-equal))
      (is (game-get-player (apply-action (make-instance 'income :by p1)
                                         g)
                           (player-id p1))
          (copy-player p1
                       :money 4)
          "Income")
      (is (game-get-player (apply-action (make-instance 'foreign-aid :by p1)
                                         g)
                           (player-id p1))
          (copy-player p1
                       :money 5)
          "Foreign Aid")
      (is (game-get-player (apply-action (make-instance 'tax :by p1)
                                         g)
                           (player-id p1))
          (copy-player p1
                       :money 6)
          "Tax")
      (is (game-get-player (apply-action (make-instance 'exchange :by p1)
                                         g)
                           (player-id p1))
          (copy-player p1
                       :cards (list (second (player-cards p1))
                                    (first (game-deck g))))
          "Exchange")
      (subtest "Assassinate"
        (plan 2)
        (let* ((g-new (apply-action (make-instance 'assassinate
                                                   :by p1
                                                   :target p2)
                                    g))
               (p1-new (game-get-player g-new (player-id p1)))
               (p2-new (game-get-player g-new (player-id p2))))
          (is p1-new (copy-player p1
                                  :money 0)
              "p1 spent 3")
          (is p2-new
              (copy-player p2
                           :cards (subseq (player-cards p2) 0 1))
              "p2 lost a card"))
        (finalize))
      (subtest "Steal"
        (plan 2)
        (subtest "Standard"
          (plan 2)
          (let* ((g-new (apply-action (make-instance 'steal
                                                     :by p1
                                                     :target p2)
                                      g))
                 (p1-new (game-get-player g-new (player-id p1)))
                 (p2-new (game-get-player g-new (player-id p2))))
            (is p1-new (copy-player p1 :money (+ (player-money p1) 2))
                "p1 gained 2")
            (is p2-new (copy-player p2 :money (- (player-money p2) 2))
                "p2 lost 2"))
          (finalize))
        (subtest "Target has less then 2 money"
          (plan 2)
          (let* ((g-new (apply-action (make-instance 'steal
                                                     :by p1
                                                     :target p3)
                                      g))
                 (p1-new (game-get-player g-new (player-id p1)))
                 (p3-new (game-get-player g-new (player-id p3))))
            (is p1-new (copy-player p1 :money (+ (player-money p1) 1))
                "p1 gained 1")
            (is p3-new (copy-player p3 :money (- (player-money p3) 1))
                "p2 lost 1"))
          (finalize))
        (finalize))
      (subtest "Coup"
        (plan 2)
        (let* ((g-new (apply-action (make-instance 'coup
                                                   :by p2
                                                   :target p1)
                                    g))
               (p1-new (game-get-player g-new (player-id p1)))
               (p2-new (game-get-player g-new (player-id p2))))
          (is p1-new (copy-player p1 :cards (cdr (player-cards p1)))
              "p1 lost a card")
          (is p2-new (copy-player p2 :money (- (player-money p2) 7))
              "p2 spent 7"))
        (finalize)))
    (subtest "Block"
      (plan 7)
      (is (apply-action (make-instance 'block-action :by p1 :target (make-instance 'income :by p2)) g)
          g
          "block income results in no change")
      (is (apply-action (make-instance 'block-action :by p1 :target (make-instance 'foreign-aid :by p2)) g)
          g
          "block foreign aid results in no change")
      (is (apply-action (make-instance 'block-action :by p1 :target (make-instance 'tax :by p2)) g)
          g
          "block tax results in no change")
      (is (apply-action (make-instance 'block-action :by p1 :target (make-instance 'exchange :by p2)) g)
          g
          "block exchange results in no change")
      (is (apply-action (make-instance 'block-action
                                       :by p1
                                       :target (make-instance 'assassinate
                                                              :by p2
                                                              :target p1))
                        g)
          g
          "block assassinate results in no change")
      (is (apply-action (make-instance 'block-action
                                       :by p1
                                       :target (make-instance 'coup
                                                              :by p2
                                                              :target p1))
                        g)
          g
          "block coup results in no change")
      (is (apply-action (make-instance 'block-action
                                       :by p1
                                       :target (make-instance 'steal
                                                              :by p2
                                                              :target p1))
                        g)
          g
          "block steal results in no change")
      (finalize))
    (finalize))

  (subtest "Illegal Actions"
    (plan 2)
    (is-error (apply-action (make-instance 'assassinate
                                           :by p3
                                           :target p1)
                            g)
              'illegal-action-error
              "Assassinate throws illegal-action-error if player-money is less than 3")
    (is-error (apply-action (make-instance 'coup
                                           :by p1
                                           :target p2)
                            g)
              'illegal-action-error
              "Coup throws illegal-action-error if player-money is less than 7")
    (finalize))

  (subtest "Testing that preceding tests did not modify players"
    (plan 3)
    (let ((prove:*default-test-function* #'players-equal))
      (is p1 p1-original "p1 unchanged")
      (is p2 p2-original "p2 unchanged")
      (is p3 p3-original "p3 unchanged")
      (finalize)))
  )

(finalize)
