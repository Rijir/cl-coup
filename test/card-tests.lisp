(in-package :coup-tests)

(plan 10)

(ok (cardp *assassin*) "assassin is a card")
(ok (cardp *contessa*) "contessa is a card")
(ok (cardp *ambassador*) "ambassador is a card")
(ok (cardp *sargent*) "sargent is a card")
(ok (cardp *duke*) "duke is a card")

(let ((deck (make-deck)))
  (is (count *assassin* deck) 3 "3 assassins in deck")
  (is (count *contessa* deck) 3 "3 contessas in deck")
  (is (count *ambassador* deck) 3 "3 ambassadors in deck")
  (is (count *sargent* deck) 3 "3 sargents in deck")
  (is (count *duke* deck) 3) "3 dukes in deck")

(finalize)
