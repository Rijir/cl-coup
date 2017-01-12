(in-package :coup-tests)

(plan 7)

(let ((p (make-instance 'player
                        :cards (list *assassin* *contessa*)
                        :money 2))
      (p2 (make-instance 'player)))
  (isnt (player-id p) (player-id p2)
        "players automatically have unique ids")
  (is (player-cards p) (list *assassin* *contessa*)
      "p has the correct cards")
  (is (player-money p) 2
      "p has the correct money")
  (ok (players-equal p (copy-player p))
      "p and a copy of p are equal")
  (ok (not (players-equal p p2))
      "p and p2 are not equal")
  (let ((p-view (player-view-from-player p)))
    (is (player-view-money p-view) 2
        "p-view has correct money")
    (is (player-view-influence p-view) 2
        "p-view has correct influence")))

(finalize)
