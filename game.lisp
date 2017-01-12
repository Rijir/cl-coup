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
  (player-views :read-only))

(defun game-view-from-game (g)
  (make-game-view :player-views (mapcar (game-players g)
                                        #'player-view-from-player)))
