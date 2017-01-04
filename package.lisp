(in-package :cl-user)
(defpackage :coup
  (:use :cl)
  (:export
   ;; cards.lisp
   *assassin*
   *contessa*
   *ambassador*
   *sargent*
   *duke*
   cardp
   make-deck
   make-deck-sorted

   ;; player.lisp
   player
   player-id
   player-cards
   player-money
   player-get-action
   player-lose-card
   player-exchange
   player-block?
   player-contest?
   copy-player
   players-equal

   ;; game.lisp
   game
   game-players
   game-deck
   make-game
   copy-game-modified
   game-get-player

   ;; actions.lisp
   illegal-action-error
   action
   income
   foreign-aid
   tax
   exchange
   block-action
   assassinate
   steal
   coup
   action-is-valid
   apply-action

   ;; coup.lisp
   init-game
   game-end
   step-game
   run-game))
