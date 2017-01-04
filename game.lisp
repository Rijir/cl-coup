(in-package :coup)

(defstruct game
  (players :read-only)
  (deck :read-only))

(defun copy-game-modified (g &key players deck)
  (make-game :players (if players players (game-players g))
             :deck (if deck deck (game-deck g))))

(defun game-get-player (g id)
  (find-if (lambda (p) (eql id (player-id p))) (game-players g)))

(defstruct game-view
  (current-player :read-only)
  (players :read-only))
