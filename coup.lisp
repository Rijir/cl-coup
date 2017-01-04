(in-package :coup)

(defun init-game (players &optional (num-copies 3))
  (let* ((deck (make-deck-shuffled num-copies))
         (players (loop for p in players
                        collect (copy-player p :cards (list (pop deck) (pop deck))
                                               :money 2))))
    (make-game :players players :deck deck)))

;; (defun game-end (g)
;;   (loop for p in (game-players g)
;;         for num-alive = (if (> (length (player-cards p)) 0)
;;                             1
;;                             0)
;;           then (+ num-alive
;;                   (if (> (length (player-cards p)) 0)
;;                       1
;;                       0))
;;         when (> num-alive 1)
;;           do (return nil))
;;   t)

(defun game-end (g)
  (<= (count-if (lambda (p)
                 (not (endp (player-cards p))))
               (game-players g))
     1))

(defun step-game (g pid)
  (break)
  (let ((p (find-if (lambda (p) (eq pid (player-id p))) (game-players g))))
    (if (> (length (player-cards p)) 0)
        (let ((a (player-get-action p g)))
          (apply-action a g))
        g)))

(defun run-game (g)
  (let ((player-ids (sort (mapcar #'player-id (game-players g))
                          #'<)))
    (if (game-end g)
        g
        (let ((game-next-round (reduce #'step-game player-ids :initial-value g)))
          (run-game game-next-round)))))
