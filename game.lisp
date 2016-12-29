(in-package :coup)

(defstruct game
  (players :read-only)
  (deck :read-only))

(defun copy-game-modified (g &key players deck)
  (make-game :players (if players players (game-players g))
             :deck (if deck deck (game-deck g))))
