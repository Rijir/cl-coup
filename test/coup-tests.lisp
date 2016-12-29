(defpackage :coup-tests
  (:use :cl))
(in-package :coup-tests)

(require :coup)

(sb-rt:deftest test-cardp-assassin
    (cardp coup:*assassin*) t)
(sb-rt:deftest test-cardp-contessa
    (cardp coup:*contessa*) t)
(sb-rt:deftest test-cardp-ambassador
    (cardp coup:*ambassador*) t)
(sb-rt:deftest test-cardp-sargent
    (cardp coup:*sargent*) t)
(sb-rt:deftest test-cardp-duke
    (cardp coup:*duke*) t)

(sb-rt:deftest test-*all-cards*-length
  (length coup:*all-cards*) 5)

(sb-rt:deftest test-actionp-income
  (actionp coup:*income*) t)
(sb-rt:deftest test-actionp-foreign-aid
  (actionp coup:*foreign-aid*) t)

(sb-rt:deftest test-make-deck-length
  (length (make-deck 3)) 15)
